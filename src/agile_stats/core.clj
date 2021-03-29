(ns agile-stats.core
  (:require [agile-stats.configs :refer [status-categories]]
            [agile-stats.db
             :refer [histogram->csv
                     load-db
                     persist-issues
                     sprints->csv
                     percentiles->csv
                     status-ages->csv]]
            [agile-stats.issue :refer [finished-issues group-by-sprints]]
            [agile-stats.jira :refer [get-issues jql-query]]
            [agile-stats.metrics
             :refer [ct-histogram
                     cycle-time-stats
                     status-time-stats
                     status-hop-stats
                     percentiles
                     status-ages]]
            [agile-stats.utils :refer [update-vals cleanup-map vec->map select-vals]]
            [clojure.data.csv :as csv]
            [java-time :as t]))

(defn update-issue-db [configs]
  (let [{:keys [base-url issue-query update-date]} configs
        {:keys [issues last-update-date] :as db} (load-db configs)
        renew-db (:renew-db configs)
        change-date (if renew-db update-date (or last-update-date update-date))
        issues (into (or (when-not renew-db issues) {})
                     (-> (str base-url jql-query issue-query)
                         (get-issues (when change-date (t/minus change-date (t/days 1))))
                         (vec->map :key)))]
    (persist-issues configs issues)
    db))

(defn- filter-by-status
  [statuses issues]
  (filter #(statuses (:status %)) issues))

(defn- sprints
  [sprint-end-date nr-sprints sprint-length finished wip-statuses]
  (group-by-sprints sprint-end-date nr-sprints sprint-length finished
                    {:ct (partial cycle-time-stats (:wip status-categories))
                     :throughput count
                     :status-times #(->> %
                                         status-time-stats
                                         (update-vals (fn [val]
                                                        (/ (:avg val) 60 24.0)))
                                         (cleanup-map wip-statuses))}))

(defn update-stats [configs]
  (let [{:keys [sprint-end-date sprint-length nr-sprints stats-file]} configs
        wip-statuses (:wip status-categories)
        issues (-> configs
                   update-issue-db
                   :issues
                   vals)
        finished (finished-issues issues (t/offset-date-time 2020 11 1))
        sprints (sprints->csv (sprints sprint-end-date nr-sprints sprint-length finished wip-statuses))
        status-hops (->> finished
                         status-hop-stats
                         (update-vals #(-> % :avg (* 1.0)))
                         (cleanup-map wip-statuses))
        percentiles (percentiles->csv (percentiles finished))
        wip-age (->> issues
                     (filter-by-status wip-statuses)
                     (status-ages wip-statuses)
                     (status-ages->csv))
        hist (histogram->csv (ct-histogram finished))]
    (with-open [writer (clojure.java.io/writer stats-file)]
      (csv/write-csv writer (-> []
                                (into sprints)
                                (into [[""][""] ["Status hops"]])
                                (into status-hops)
                                (into [[""]["Percentiles"]])
                                (into percentiles)
                                (into [[""] [""] ["WIP Age"]])
                                (into wip-age)
                                (into [[""] [""] ["Cycle Time Histogram"]])
                                (into hist))))))

(defn default-sprints [issues]
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
