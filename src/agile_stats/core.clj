(ns agile-stats.core
  (:require [agile-stats.db
             :refer [histogram->csv
                     load-db
                     persist-issues
                     persist-db
                     sprints->csv
                     percentiles->csv
                     status-ages->csv
                     csv-transpose]]
            [agile-stats.issue :refer [finished-issues group-by-sprints update-issue-stats]]
            [agile-stats.jira :refer [get-issues jql-query update-details]]
            [agile-stats.metrics
             :refer [ct-histogram
                     cycle-time-stats
                     status-time-stats
                     status-hop-stats
                     detailed-hops
                     ct-percentiles
                     status-ages
                     throughput-per-day
                     monte-carlo-issues
                     monte-carlo-time
                     ]]
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
  (let [{:keys [base-url issue-query update-date basic-auth]} configs
        {:keys [issues last-update-date] :as db} (load-db configs)
        renew-db (:renew-db configs)
        change-date (if renew-db update-date (or last-update-date update-date))
        issues (into (or (when-not renew-db issues) {})
                     (-> base-url
                         (get-issues basic-auth issue-query update-date change-date)
                         (vec->map :key)))]
    (persist-issues configs issues)))

(defn refresh-issue-details [configs]
  (let [db (load-db configs)
        issues (->> db
                   :issues
                   vals
                   (update-details (:base-url configs)))
        issues (vec->map issues :key)]
    (->> issues
         (assoc db :issues)
         (persist-db configs))))

(defn- sprints
  [sprint-end-date nr-sprints sprint-length finished wip-statuses]
  (group-by-sprints sprint-end-date nr-sprints sprint-length finished
                    {:ct (partial cycle-time-stats wip-statuses)
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
                                     {:to "foo" :date (t/offset-date-time 2021 1 4)}
                                     {:to "bla" :date (t/minus (t/offset-date-time) (t/days 2))}])))

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
  (let [{:keys [sprint-end-date sprint-length nr-sprints stats-file update-date mc-nr-issues mc-days status-categories]
         :or {sprint-length 2
              nr-sprints 8
              sprint-end-date (t/offset-date-time)}} configs
        weeks-back (* nr-sprints sprint-length)
        update-date (or update-date (t/minus sprint-end-date (t/weeks weeks-back)))
        wip-statuses (:wip status-categories)
        issues (map (partial update-issue-stats status-categories)
                    (-> configs
                        update-issue-db
                        :issues
                        vals))
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
                     (filter #(wip-statuses (:status %)))
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
        blocked (->> finished
                     (filter #(and (:done-date %)
                                   (t/before? update-date
                                              (:done-date %))
                                   (get-in % [:stats :status-times "Blocked"])))
                     (reduce #(-> [(str (first %) "," (:key %2)
                                        ": " (-> %2
                                                 (get-in [:stats :status-times "Blocked" :duration])
                                                 minutes->days))
                                   (str (second %) ","(:key %2))])
                             ["" ""]))
        ref-throughput (throughput-per-day update-date (t/offset-date-time) finished)
        mc-issues (when mc-days
                    (->> ref-throughput
                         (monte-carlo-issues mc-days)
                         percentiles->csv))
        mc-time (when mc-nr-issues
                  (->> ref-throughput
                       (monte-carlo-time mc-nr-issues (t/offset-date-time))
                       (percentiles->csv)))
        wip-size (csv-transpose (into (sorted-map-by t/before?)
                                      (filter #(t/before? (t/local-date update-date) (key %))
                                              (wip-size-per-day wip-statuses issues)))
                                {:format-first #(t/format "YYYY-MM-dd" %)})
        hop-details (->> finished
                         (filter #(let [value  (get-in % [:stats :ct])
                                        days (if value
                                               (agile-stats.utils/minutes->days value) 0) ]
                                    (and (> days 7)
                                         (t/after? (:done-date %) (t/offset-date-time 2021 3 1)))))
                         (detailed-hops wip-statuses)
                         (reduce #(-> %
                                      (conj [])
                                      (conj (select-vals %2 [:key :summary :ct :estimate]) ;(butlast (butlast %2))
                                            )
                                      (conj (:durations %2))
                                      (into (:hops %2))) []))
        hist (histogram->csv (ct-histogram finished))]
    (with-open [writer (clojure.java.io/writer stats-file)]
      (csv/write-csv writer (-> []
                                ;(into [(into ["Blocked"] blocked)])
                                (into [(into ["Done last sprint"] done-issues)])
;                                (into (vec (update-vals count (group-by :issuetype (-> sprints last :issues)))))
                                (into [[""]])
                                (into csv-sprints)
                                (into [[""]["Cycle Time Percentiles (days)"]])
                                (into percentiles)
                                (into (when mc-days[[""] [(str "Monte Carlo Issue Count (" mc-days " days)")]]))
                                (into (when mc-days mc-issues))
                                (into (when mc-nr-issues [[""][(str "Monte Carlo Done dates for " mc-nr-issues " issues")]]))
                                (into (when mc-nr-issues mc-time))
                                (into [[""] [""] ["WIP Age"]])
                                (into wip-age)
                                (into [[""] ["WIP-Size"]])
                                (into wip-size)
                                (into [[""][""] ["Status hops"]])
                                (into status-hops)
                                (into [[""]["Status hops details"]])
                                (into hop-details)
                                (into [[""] [""] ["Cycle Time Histogram"]])
                                (into hist))))))

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
        percentiles (map #(let [before-date (t/minus sprint-end-date (t/days (* % sprint-length)))
                                after-date (t/minus before-date (t/weeks 8))
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
