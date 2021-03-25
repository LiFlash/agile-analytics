(ns agile-stats.core
  (:require [agile-stats.configs :refer [status-categories]]
            [agile-stats.db
             :refer
             [histogram->csv load-db persist-issues sprints->csv]]
            [agile-stats.issue :refer [finished-issues group-by-sprints]]
            [agile-stats.jira :refer [get-issues jql-query]]
            [agile-stats.metrics
             :refer
             [ct-histogram cycle-time-stats status-time-stats]]
            [agile-stats.utils :refer [apply-to-vals cleanup-map vec->map]]
            [clojure.data.csv :as csv]
            [java-time :as t]))

(defn update-issue-db [configs]
  (let [{:keys [base-url issue-query update-date]} configs
        {:keys [issues last-update-date] :as db} (load-db configs)
        change-date (if (:renew-db configs) update-date (or last-update-date update-date))
        issues (into (or issues {})
                     (-> (str base-url jql-query issue-query)
                         (get-issues (when change-date (t/minus change-date (t/days 1))))
                         (vec->map :key)))]
    (persist-issues configs issues)
    db))

(defn update-stats [configs]
  (let [{:keys [sprint-end-date sprint-length nr-sprints stats-file]} configs
        issues (-> configs
                   update-issue-db
                   :issues
                   vals)
        finished (finished-issues issues (t/offset-date-time 2020 11 1))
        sprints (sprints->csv (group-by-sprints
                               sprint-end-date nr-sprints sprint-length finished
                               {:ct (partial cycle-time-stats (:wip status-categories))
                                :throughput count
                                :status-times #(->> %
                                                   status-time-stats
                                                   (apply-to-vals (fn [val]
                                                                    (/ (:avg val) 60 24.0)))
                                                   (cleanup-map (:wip status-categories)))
                                ;; :status-hops #(-> %
                                ;;                   status-hop-stats
                                ;;                   (apply-to-vals :avg))
                                }))
        hist (histogram->csv (ct-histogram finished))]
    (with-open [writer (clojure.java.io/writer stats-file)]
      (csv/write-csv writer (-> []
                                (into sprints)
                                (into [[""][""]])
                                (into hist))))))

(defn sprints [issues]
  (group-by-sprints (t/offset-date-time 2021 3 16) 7 2
                    (finished-issues issues)
                    {:ct (partial cycle-time-stats (:wip status-categories))
                     :throughput count
                     :status-times #(cleanup-map (:wip status-categories)
                                                 (reduce (fn [r status]
                                                           (assoc r (first status)
                                                                  (:sum (second status))))
                                                         {} (status-time-stats %)))}))

;(def ds-issues (update-issue-db ds))
;(def cch-issues (update-issue-db cch))
