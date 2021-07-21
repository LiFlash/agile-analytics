(ns agile-stats.issue
  (:require [agile-stats.utils :refer [select-vals update-vals]]
            [java-time :as t]))

;; Bsp. Issue
(comment
  {:self <weblink to the jira issue>
   :key "BSP-001"
   :issuetype "Type of the issue"
   :summary "Issue summary"
   :created-date :date-of-creation
   :created-status :initial-status ;;TODO Find out the correct one. Currently we assume it's always "To be defined"
   :status :current-status
   :done-date :date-of-finishing-work
   :estimate "Original Estimate"
   :transitions [{:from :some-status
                  :to :another-status
                  :date :date-of-transition}]
   :stats {:status-times {:initial-status {:duration "time in status"
                                           :times-reached "nr of times this status was set"
                                           :last-reached "date this status was reached the last time"}
                          :another-status {:duration :x, :times-reached :y, :last-reached :z}}
           :ct "cumulated time in a given set of statuses"}})

(defn done-date [done-statuses issue]
  ;;TODO Kann performanter gemacht werden, wenn die :stats eines issues vorausgesetzt werden
  (when (get done-statuses (:status issue))
    (->> issue
         :transitions
         (reduce (fn [result {:keys [date to] :as transition}]
                   (when (done-statuses to)
                     (or result date))) nil))))

(defn update-done-date [done-statuses issue]
  (assoc issue :done-date (done-date done-statuses issue)))

;; (defn group-by-status [issues]
;;   (reduce #(merge-maps % (get-in %2 [:stats :status-times])
;;                        (fn [update val]
;;                          ((fnil conj []) update %2))) {} issues))

(defn resolved? [issue]
  (when (:done-date issue) issue))

(defn had-status? [status issue]
  (when (-> issue :stats :status-times (get status))
    issue))

(defmulti status-times
  "For multiple issues returns a map containing the statuses and durations for all issues. For a single issue the timely stats are extracted from the transitions. i.e. The time an issue was in a status, the last date an issue visited a status, etc."
  map?)

(defmethod status-times true
  [issue]
  ;;TODO refactor input param to be transitions not a whole issue
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
                   (:to transition)
                   times)))
        times))))

(defmethod status-times :default
  [issues]
  (->> issues
       (map #(->> [:stats :status-times]
                  (get-in % )
                  (update-vals :duration)
                  (update-vals vector)))
       (apply merge-with into {})))

(defn status-hops
  [issues]
  (->> issues
       (map #(->> [:stats :status-times]
                  (get-in %)
                  (update-vals :times-reached)
                  (update-vals vector)))
       (apply merge-with into {})))

(defn update-status-times
  "Assumes transitions to be sorted by date from earliest to latest"
  [issue]
  (assoc-in issue [:stats :status-times] (status-times issue)))

(defn cycle-time
  "Cycle Time for a single issue"
  [statuses issue]
  (reduce #(+ % (:duration %2)) 0
          (select-vals (get-in issue [:stats :status-times]) statuses)))

(defn update-cycle-time [statuses issue]
  ;;TODO Remove this one (and all access to [:stats :ct]) and replace by always
  ;; calculating ct for the needed statuses instead of saving it here, because
  ;; ct is dependent on the given statuses
  ;; Vielleicht auch nicht sinnvoll dies zu entfernen, da der Aufrufer am Ende die Verknuepfung von
  ;; issue und :ct braucht. cycle-time muss auf jeden Fall aus der persistenz raus.
  (assoc-in issue [:stats :ct] (cycle-time statuses issue)))

(defn last-transition-date [issue]
  (-> issue :transitions last :date))

(defn update-last-transition-date [issue]
  (assoc issue :last-transition-date (last-transition-date issue)))

(defn update-issue-stats [status-categories issue]
  (->> issue
       update-status-times
       (update-done-date (:done status-categories))
       update-last-transition-date
       (update-cycle-time (:wip status-categories))))

(defn resolved-between [start end issues]
  (reduce (fn [r issue]
            (if-let [done-date (:done-date (resolved? issue))]
              (if (t/before? start done-date end)
                (conj r issue)
                r)
              r)) [] issues))

(defn finished-issues
  "Issues that are done (after after-date and before before-date if given) and were worked on, i.e. cycle time > 0"
  [issues & [after-date before-date]]
  (filter #(and (resolved? %)
                (> (get-in % [:stats :ct]) 0)
                (or (not after-date) (t/before? after-date (:done-date %)))
                (or (not before-date) (t/before? (:done-date %) before-date)))
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

(defn age
  "Returns the cycle time for statuses + the time the issue is in it's current
  status if it is one of statuses"
  [statuses issue]
  (let [ct (cycle-time statuses issue)]
    (if (statuses (:status issue))
      (let [date  (:last-transition-date issue)
            time-in-status (t/time-between date (t/offset-date-time) :minutes)]
        (t/plus ct time-in-status))
      ct)))

(defn update-age [statuses issue]
  (assoc-in issue [:stats :age] (age statuses issue)))

(defn status-durations [statuses issue]
  ;;TODO extract statuses as a parameter
  ;;TODO remove conversion to days
  ;;TODO remove the filtering for durations > 1 day. This should be done by the caller, if he is only interested in those
  (->> [:stats :status-times]
       (get-in issue)
       (filter #(and (->> %
                          key
                          (contains? statuses))
                     (->> %
                          val
                          :duration
                          agile-stats.utils/minutes->days
                          (< 1))))
       (map #(str (key %) ": " (-> %
                                   val
                                   :duration
                                   agile-stats.utils/minutes->days)))))

(defn status-visits [issue]
  ;;TODO remove optimization for CSV, like date string and conversion to days instead of minutes
  (let [hops (:transitions issue)]
    (last (reduce (fn [[last-hop hops] hop]
                    (if last-hop
                      (let [status (:to last-hop)
                            date (:date last-hop)
                            duration (-> date
                                         (t/time-between (:date hop) :minutes)
                                         agile-stats.utils/minutes->days)]
                        [hop ((fnil conj []) hops [(str (t/format "dd.MM.YY HH:mm: " date)) status duration])])
                      [hop]))
                  [] hops))))
