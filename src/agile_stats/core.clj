(ns agile-stats.core
  (:require [agile-stats.configs :refer [status-categories]]
            [agile-stats.db
             :refer [histogram->csv
                     load-db
                     persist-issues
                     sprints->csv
                     percentiles->csv
                     status-ages->csv
                     csv-transpose]]
            [agile-stats.issue :refer [finished-issues group-by-sprints]]
            [agile-stats.jira :refer [get-issues jql-query]]
            [agile-stats.metrics
             :refer [ct-histogram
                     cycle-time-stats
                     status-time-stats
                     status-hop-stats
                     ct-percentiles
                     status-ages
                     throughput-per-day
                     monte-carlo-issues
                     monte-carlo-time]]
            [agile-stats.utils
             :refer [update-vals
                     cleanup-map
                     vec->map
                     select-vals
                     minutes->days
                     round]]
            [clojure.data.csv :as csv]
            [java-time :as t]))

(defn update-issue-db [configs]
  (let [{:keys [base-url issue-query update-date]} configs
        {:keys [issues last-update-date] :as db} (load-db configs)
        renew-db (:renew-db configs)
        change-date (if renew-db update-date (or last-update-date update-date))
        issues (into (or (when-not renew-db issues) {})
                     (-> (str base-url jql-query issue-query)
                         (get-issues update-date change-date)
                         (vec->map :key)))]
    (persist-issues configs issues)))

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
                                                        (minutes->days (:avg val))))
                                         (cleanup-map wip-statuses))}))

(defn create-days-between [date1 date2]
  (let [nr-days (t/time-between date1 date2 :days)]
    (->> (t/local-date date1)
         (iterate #(t/plus % (t/days 1)))
         (take nr-days))))

(defn days-in-statuses
  "Returns a collection of dates at which the transitions had one of the given statuses"
  [statuses transitions]
  (loop [last-t (first transitions)
         next-t (second transitions)
         ts (drop 2 transitions)
         dates []]
    (if next-t
      (let [dates (if (contains? (set statuses) (:to last-t))
                    (->> next-t
                         :date
                         (create-days-between (:date last-t))
                         (into dates))
                    dates)]
        (recur next-t (first ts) (rest ts) dates))
      (if (contains? (set statuses) (:to last-t))
        (set (into dates (create-days-between (:date last-t) (t/offset-date-time))))
        (set dates)))))

(comment
  (defn test-days-in-statuses []
    (days-in-statuses ["bla" "blub"][{:to "bla" :date (t/offset-date-time 2021 1 1)}
                                     {:to "foo" :date (t/offset-date-time 2021 1 2)}
                                     {:to "blub" :date (t/offset-date-time 2021 1 3)}
                                     {:to "foo" :date (t/offset-date-time 2021 1 4)}])))

(defn wip-size-per-day [wip-statuses issues]
  (reduce #(let [days (days-in-statuses wip-statuses (:transitions %2))]
             (reduce (fn [r day]
                       (update r day (fnil inc 0))) % days))
          {} issues))

(comment
  (wip-size-per-day ["bla" "blub"]
                    [{:transitions
                      [{:to "bla" :date (t/offset-date-time 2021 1 1)}
                       {:to "foo" :date (t/offset-date-time 2021 1 2)}
                       {:to "blub" :date (t/offset-date-time 2021 1 3)}
                       {:to "foo" :date (t/offset-date-time 2021 1 5)}]}
                     {:transitions
                      [{:to "bla" :date (t/offset-date-time 2021 1 1)}
                       {:to "foo" :date (t/offset-date-time 2021 1 3)}
                       {:to "blub" :date (t/offset-date-time 2021 1 4)}
                       {:to "foo" :date (t/offset-date-time 2021 1 5)}]}]))

(defn update-stats [configs]
  (let [{:keys [sprint-end-date sprint-length nr-sprints stats-file update-date]} configs
        wip-statuses (:wip status-categories)
        issues (-> configs
                   update-issue-db
                   :issues
                   vals)
        finished (finished-issues issues update-date)
        sprints  (sprints sprint-end-date nr-sprints sprint-length finished wip-statuses)
        csv-sprints (sprints->csv sprints)
        status-hops (->> finished
                         status-hop-stats
                         (update-vals #(->> % :avg (* 1.0) (round 2)))
                         (cleanup-map wip-statuses))
        percentiles (percentiles->csv (->> finished
                                           ct-percentiles
                                           (update-vals minutes->days)))
        wip-age (->> issues
                     (filter-by-status wip-statuses)
                     (status-ages wip-statuses)
                     (status-ages->csv))
        done-issues (->> sprints
                         last
                         :issues
                         (status-ages wip-statuses)
                         vals flatten
                         (sort-by #(get-in % [:stats :age]))
                         (map #(str (:key %)
                                        ;", " (:summary %)
                                    ", " (minutes->days (get-in % [:stats :age])))))
        blocked (reduce #(str % "," (:key %2)
                              ": " (get-in [:stats :status-times "Blocked" :duration] %2)) ""
                        (filter #(and (:done-date %)
                                      (t/after? (t/minus (t/offset-date-time) (t/weeks 2)) (:done-date %))
                                      (get-in % [:stats :status-times "Blocked"]))
                                finished))
        ref-throughput (throughput-per-day  (t/minus (t/offset-date-time) (t/weeks 14)) (t/offset-date-time) finished)
        nr-issues 40
        mc-issues (->> ref-throughput
                       (monte-carlo-issues 14)
                       percentiles->csv)
        mc-time (->> ref-throughput
                     (monte-carlo-time nr-issues (t/offset-date-time))
                     (percentiles->csv))
        hist (histogram->csv (ct-histogram finished))
        wip-size (csv-transpose (into (sorted-map-by t/before?)
                                      (filter #(t/before? (t/local-date 2021 1 1) (key %))
                                              (wip-size-per-day wip-statuses issues)))
                                {:format-first #(t/format "YYYY-MM-dd" %)})]
    (with-open [writer (clojure.java.io/writer stats-file)]
      (csv/write-csv writer (-> []
                                ;; (into [["Blocked"
                                ;;         blocked]])
                                (into [(into ["Done last sprint"] done-issues)])
                                (into [[""]])
                                (into csv-sprints)
                                (into [[""][""] ["Status hops"]])
                                (into status-hops)
                                (into [[""]["Cycle Time Percentiles (days)"]])
                                (into percentiles)
                                (into [[""]["Monte Carlo Issue Count (14 days)"]])
                                (into mc-issues)
                                (into [[""][(str "Monte Carlo Done dates for " nr-issues " issues")]])
                                (into mc-time)
                                (into [[""] [""] ["WIP Age"]])
                                (into wip-age)
                                (into [[""] ["WIP-Size"]])
                                (into wip-size)
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
(defn ct-percentiles-history [configs]
  (let [{:keys [sprint-end-date sprint-length nr-sprints stats-file update-date]} configs
        issues (-> configs
                   load-db
                   :issues
                   vals)
        finished (finished-issues issues update-date)
        percentiles (percentiles->csv (->> finished
                                            ct-percentiles
                                            (update-vals minutes->days)))
        percentiles (map #(let [before-date (t/minus sprint-end-date (t/days (* % 14)))
                                after-date (t/minus before-date (t/weeks 15))
                                finished (finished-issues issues after-date before-date)]
                            [before-date (second (percentiles->csv (->> finished
                                                                        ct-percentiles
                                                                        (update-vals minutes->days))))])
                         (range nr-sprints))]
    (println percentiles)
    ;; (with-open [writer (clojure.java.io/writer "percentiles-history.csv")]
    ;;   (csv/write-csv writer (-> []
    ;;                             ;; (into [["Blocked"
    ;;                             ;;         blocked]])

    ;;                             (into [[""]["Cycle Time Percentiles (days)"]])
    ;;                             (into percentiles))))
    ))
