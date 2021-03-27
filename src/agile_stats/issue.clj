(ns agile-stats.issue
  (:require [agile-stats.utils :refer [merge-maps select-vals update-vals]]
            [java-time :as t]))

;; Bsp. Issue
(comment
  {:self <weblink to jira>
   :key "BSP-001"
   :created-date :date-of-creation
   :created-status :initial-status
   :status :current-status
   :done-date :date-of-finishing-work
   :transitions [{:from :some-status
                  :to :another-status
                  :date :date-of-transition}]
   :stats {:status-times {:initial-status {:duration "time in status"
                                           :times-reached "nr of times this status was set"
                                           :last-reached "date this status was reached the last time"}
                          :another-status {:duration :x, :times-reached :y, :last-reached :z}}
           :ct "cummulated time in a given set of statuses"}})

(defn done-date [done-statuses issue]
  ;;TODO Abstrahieren: Funktioniert fuer jede Statuskategorie
  ;;TODO Kann performanter gemacht werden, wenn die :stats eines issues vorausgesetzt werden
  (when (get done-statuses (:status issue))
    (->> issue
         :transitions
         (reduce (fn [result {:keys [date to] :as transition}]
                   (if (get done-statuses to)
                     (or result date)
                     nil)) nil))))

(defn update-done-date [done-statuses issue]
  (assoc issue :done-date (done-date done-statuses issue)))

(defn group-by-status [issues]
  (reduce #(merge-maps % (get-in %2 [:stats :status-times])
                       (fn [update val]
                         ((fnil conj []) update %2))) {} issues))

(defn resolved? [issue]
  (when (:done-date issue) issue))

(defn had-status? [status issue]
  (when (-> issue :stats :status-times (get status))
    issue))

(defmulti status-times
  "For multiple issues returns a map containing the statuses and durations for all issues."
  class)

(defmethod status-times clojure.lang.PersistentArrayMap
  [issue]
  ;;TODO Refactor loop -> reduce
  ;;TODO refactor input param to be transitions not a hole issue
  (let [{:keys [created-status created-date transitions]} issue]
    (loop [transitions transitions
           last-date created-date
           last-status created-status
           times {}]
      (if-let [transition (first transitions)]
        (let [to-date (:date transition)
              duration (t/time-between last-date to-date :minutes)]
          (if (< 0 duration)
            ;;skip current transition, if it didn't stay longer than a minute
            (recur (rest transitions)
                   to-date
                   (:to transition)
                   (-> times
                       (update-in [last-status :duration] (fnil + 0) duration )
                       (update-in [last-status :times-reached] (fnil inc 0))
                       (assoc-in [last-status :last-reached] to-date)))
            (recur (rest transitions)
                   last-date
                   last-status
                   times)))
        times))))

(defmethod status-times :default
  [issues]
  (->> issues
       (map #(->> [:stats :status-times]
                  (get-in % )
                  (update-vals :duration)))
       (apply merge-maps (fnil conj []) {})))

(defn status-hops
  [issues]
  (->> issues
       (map #(->> [:stats :status-times]
                  (get-in %)
                  (update-vals :times-reached)))
       (apply merge-maps (fnil conj []) {})))

(defn update-status-times
  "Assumes transitions to be sorted by date from earliest to latest"
  [issue]
  (assoc-in issue [:stats :status-times] (status-times issue)))

(defn cycle-time
  "Cycle Time for a single issue"
  [statuses issue]
  (reduce #(+ % (:duration %2)) 0 (select-vals (get-in issue [:stats :status-times]) statuses)))

(defn update-cycle-time [statuses issue]
  (assoc-in issue [:stats :ct] (cycle-time statuses issue)))

(defn update-issue-stats [ status-categories issue]
  (->> issue
       update-status-times
       (update-done-date (:done status-categories))
       (update-cycle-time (:wip status-categories))))

(defn resolved-between [start end issues]
  (reduce (fn [r issue]
            (if-let [done-date (:done-date (resolved? issue))]
              (if (t/before? start done-date end)
                (conj r issue)
                r)
              r)) [] issues))

(defn finished-issues
  "Issues that are done (after after-date if given) and were worked on, i.e. cycle time > 0"
  [issues & [after-date]]
  (filter #(and (resolved? %)
                (> (get-in % [:stats :ct]) 0)
                (or (not after-date) (t/before? after-date (:done-date %))))
          issues))

(defn group-by-sprints [sprint-end-date nr-sprints sprint-length issues & [stat-fns]]
  (let [sprints (-> nr-sprints range reverse)]
    (map #(let [end-date (t/minus sprint-end-date (t/weeks (* sprint-length %)))
                start-date (t/plus (t/minus end-date (t/weeks sprint-length)) (t/days 1))
                sprint-issues (resolved-between start-date end-date issues)
                sprint {:end-date end-date, :start-date start-date
                        :issues sprint-issues}]
            (if stat-fns
              (reduce (fn[sprint [key stat-fn]]
                        (assoc sprint key (stat-fn sprint-issues))) sprint stat-fns)
              sprint))
         sprints)))
