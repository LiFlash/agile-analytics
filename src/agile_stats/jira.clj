(ns agile-stats.jira
  (:require [clojure.data.csv :as csv]
            [clj-http.client :as client]
            [java-time :as t]))

(def status-categories {:todo #{"To Do" "Dependent" "Can be groomed" "To be defined"}
                        :wip #{"Blocked" "To Be Fixed" "In Progress" "R4 Code Review" "Code Review" "R4 Testing" "Testing" }
                        :done #{"R4 Merge" "Approval" "Merge" "R4 Release" "Done"}})

(defn jira-get [query]
;  (println query)
  (:body (client/get query
                     {:basic-auth ["johannes.graessler@digistore24.team" "pVcDoBNcl82d8i0rYSjI2033"]
                      :accept :json
                      :as :json})))

(defn jira-put [url body]
  (:body (client/put url {:basic-auth ["johannes.graessler@digistore24.team" "pVcDoBNcl82d8i0rYSjI2033"]
                          :content-type :json
                          :form-params body})))

(defn isLast? [{:keys [maxResults issues values] :as response}]
  (< (count (or issues values)) maxResults))

(defn nextPage [query {:keys [startAt maxResults] :as response}]
  (let [appender (if (.contains query "?") "&" "?")]
    (str query appender "startAt=" (+ startAt maxResults))))

(defn jira-paginate [query]
  (loop [current-query query
         values []]
    (let [resp (jira-get current-query)
          values (into values (or (:issues resp) (:values resp)))]
      (if (not (isLast? resp))
        (recur (nextPage query resp) values)
        values))))

(defn jira-changelog [issue-key]
  (jira-paginate (str "https://digistore.atlassian.net/rest/api/2/issue/" issue-key "/changelog")))

(defn adjust-date-string [s]
  (let [index 26]
    (str (subs s 0 index) ":" (subs s index))))

(defn str->date [s]
  (-> s adjust-date-string t/offset-date-time))

(defn changelog [response]
  (-> response :changelog :histories))

(defn transition? [changelog-entry]
  (loop [changes (:items changelog-entry)]
    (let [change (first changes)]
      (when change
        (if (= "status" (:field change))
          true
          (recur (rest changes)))))))

(defn parse-transition [entry]
  (reduce #(if (= "status" (:field %2))
             {:date (-> entry :created str->date)
              :from (:fromString %2)
              :to (:toString %2)}
             %)
          nil (:items entry)))

(defn transition-entries [changelog]
  (->> changelog
       (filter transition?)
       (map parse-transition)))

(defn parse-issue [issue]
  (do {:self (str "https://digistore.atlassian.net/browse/" (:key issue))
       :key (:key issue)
       :created-date (-> issue (get-in [:fields :created]) str->date)
       :created-status "To be defined"
       :status (get-in issue [:fields :status :name])
       :transitions (->> issue :key jira-changelog transition-entries (sort-by :date t/before?))}))

(defn resolved? [issue]
  (:done-date issue))

(defn had-status? [status issue]
  (when (-> issue :stats :status-times (get status))
    issue))

(defn merge-maps
  "Merges m2 into m1 using f as the merge function. f should accept the value to
  be updated from m1 and the appropriate value from m2 as arguments."
  [m1 m2 f]
  (reduce  #(update % (first %2) f (second %2)) m1 m2))

(defn group-by-status [issues]
  (reduce #(merge-maps % (get-in %2 [:stats :status-times])
                       (fn [update val]
                         ((fnil conj []) update %2))) {} issues))

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
                       (assoc :last-transition-date to-date)
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
  (reduce (fn [r issue]
            (merge-maps r (get-in issue [:stats :status-times])
                        #(let [duration (:duration %2)]
                           (if duration
                             ((fnil conj []) % duration)
                             %))))
          {} issues))

(defn update-status-times
  "Assumes transitions to be sorted by date from earliest to latest"
  [issue]
  (assoc-in issue [:stats :status-times] (status-times issue)))

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

(defn median [ns]
  (if (not (empty? ns))
    (let [ns (sort ns)
          cnt (count ns)
          mid (bit-shift-right cnt 1)]
      (if (odd? cnt)
        (nth ns mid)
        (/ (+ (nth ns mid) (nth ns (dec mid))) 2)))
    0))

(defn avg [vs]
  (if (not (empty? vs))
    (/ (apply + vs) (count vs))
    0))

(defn key-vals
  "returns the vals for keys ks ommitting nil values."
  [m ks]
  (reduce #(if-let [v (get m %2)]
             (conj % v) %) [] ks))

(defn cycle-time
  "Cycle Time for a single issue"
  [statuses issue]
  (reduce #(+ % (:duration %2)) 0 (key-vals (get-in issue [:stats :status-times]) statuses)))

(defn update-cycle-time [statuses issue]
  (assoc-in issue [:stats :ct] (cycle-time statuses issue)))

(defn stats [vs]
  {:avg (avg vs)
   :median (median vs)
   :sum (apply + vs)})

(defn cycle-time-stats
  "Avg. and Median cycle times over all issues. Arity one assumes the key [:stats :ct] exists for each issue"
  ([issues]
   (let [cycle-times (map #(get-in % [:stats :ct]) issues)]
     {:ct-sum (apply + cycle-times)
      :avg (avg cycle-times)
      :median (median cycle-times)}))
  ([statuses issues]
   (let [issues (map #(update-cycle-time statuses %) issues)]
     (cycle-time-stats issues))))

;; (cycle-time-stats [:ip :review] [{:stats {:status-times {:ip {:duration 4}
;;                                                          :review {:duration 2}}}}
;;                                  {:stats {:status-times {:ip {:duration 5}
;;                                                          :review {:duration 11}}}}])

(defn status-stats
  "Returns a map with stats like avg, median and sum for each status the issues had."
  [issues]
  (reduce (fn [r [status times]]
            (if (not (empty? times))
              (assoc r status (stats times))
              r))
          {} (status-times issues)))

;; (status-stats [{:stats {:status-times {:ip {:duration 4}
;;                                        :last-transition-date "Y"
;;                                        :review {:duration 2}}}}
;;                {:stats {:status-times {:ip {:duration 5}
;;                                        :last-transition-date "X"
;;                                        :review {:duration 11}}}}])

(defn update-issue-stats [issue]
  (->> issue
       update-status-times
       (update-done-date (:done status-categories))
       (update-cycle-time (:wip status-categories))))

(defn resolved-between [start end issues]
  (reduce (fn [r issue]
            (if-let [done-date (resolved? issue)]
              (if (t/before? start done-date end)
                (conj r issue)
                r)
              r)) [] issues))

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

(defn finished-issues
  "Issues that are done (after after-date if given) and were worked on, i.e. cycle time > 0"
  [issues & [after-date]]
  (filter #(and (resolved? %)
                (> (get-in % [:stats :ct]) 0)
                (or (not after-date) (t/before? after-date (:done-date %))))
          issues))

(defn ct-histogram [issues]
  (let [hist (group-by #(Math/round (/ (get-in % [:stats :ct]) 60 24.0)) issues)
        max-time (apply max (keys hist))
        times (range (inc max-time))]
    (reduce #(assoc % %2 (hist %2)) {} times)))

;(-> issues (finished-issues (t/offset-date-time 2020 11 1)) ct-histogram)

(defn status-times->csv [sprints]
  (let [times (reduce (fn [r sprint]
                        (reduce #(update % (first %2) (fnil conj []) (second %2)) r (:status-times sprint)))
                      {} sprints)]
    (into [] (mapv #(into [(first %)] (second %)) times))))

;; (let [sprints [{:status-times {"To Do" 5, "Done" 3}}
;;                {:status-times {"To Do" 2, "Done" 7}}]]
;;   (status-times->csv sprints))

(defn sprints->csv [sprints]
  (let [stats (reduce (fn [r sprint]
                        (let [stats (-> r
                                        (update :end-date conj (t/format "YYYY-MM-dd"(:end-date sprint)))
                                        (update :throughput conj (:throughput sprint))
                                        (update :avg conj (/ (get-in sprint [:ct :avg]) 60 24.0))
                                        (update :median conj (/ (get-in sprint [:ct :median]) 60 24.0)))]
                          stats))
                      {:end-date [], :throughput [], :avg [], :median []}
                      sprints)]
    (into [(into ["Sprintende"] (:end-date stats))
           (into ["Throughput"] (:throughput stats))
           (into ["Cycle Time Avg"] (:avg stats))
           (into ["Cycle Time Median"] (:median stats))
           []["Times in statuses"]]
          (status-times->csv sprints))))

(defn histogram->csv [histogram]
  (->> histogram
       (map #(-> [(first %) (count (second %))]
                 (into (map :key (second %)))))
       (sort-by first <)
       (into [["Cycle Time (days)" "#Issues" "Issues"]])))

(def base-url "https://digistore.atlassian.net/rest/api/2/")
(def query-url (str base-url "search?jql="))


(defn get-issues [query & [update-date]]
  (let [issues (jira-paginate (str query-url
                                   query
                                   (and update-date (str " and updated > " (t/format "YYYY-MM-dd" update-date)))
                                   "&fields=created,status" ))]
    (->> issues
         (map parse-issue)
         (map update-issue-stats))))

(defn read-issue-edn [file-path]
  (when (.exists (clojure.java.io/file file-path))
    (clojure.edn/read-string
     {:readers {'object #(t/offset-date-time (last %))}}
     (slurp file-path))))

(def ds {:sprint-end-date (t/offset-date-time 2021 3 12)
         ;:update-date (t/offset-date-time 2020 12 1);(t/offset-date-time 2020 11 1)
         :sprint-length 2
         :nr-sprints 7
         :storage-file "ds-issues.edn"
         :stats-file "ds-stats.csv"
         :update-query "project = DS AND type not in (\"Test Execution\",
 \"Test Plan\", \"Test Set\", \"Xray Test\",
 \"Sub Test Execution\", Precondition, Sub-Bug, Sub-Task, Epic, Bug) AND labels in (DS_Frontend,DS_Backend)"})

(def cch {:sprint-end-date (t/offset-date-time 2021 3 16)
          :sprint-length 2
          :nr-sprints 7
 ;         :update-date (t/offset-date-time 2020 12 1)
          :storage-file "cch-issues.edn"
          :stats-file "cch-stats.csv"
          :update-query "project in (OCB,PGB) and status was not done before \"2020-12-01\" AND issuetype in (Task, Story)"})

(defn vec->map [v k]
  (reduce #(assoc % (get %2 k) %2) {} v))

(defn load-update-issues
  ([configs] (load-update-issues (:update-query configs) (:storage-file configs) (:update-date configs)))
  ([update-query storage-file & [update-date]]
   (let [{:keys [issues last-update-date]} (read-issue-edn storage-file)
         issues (into (or issues {})
                      (-> update-query
                          (get-issues (when-let [date (or update-date last-update-date)]
                                        (t/minus date (t/days 1))))
                          (vec->map :key)))
         store {:last-update-date (t/offset-date-time), :issues issues}]
     (spit storage-file (prn-str store))
     (vals issues))))

(def ds-issues (load-update-issues ds))
(def cch-issues (load-update-issues cch))

;; (spit "ds-issues.edn"
;;       {:issue (map update-issue-stats issues)
;;        :last-update-date (t/offset-date-time 2021 3 7)})

;(-> issues finished-issues status-times)

(defn cleanup-map
  "Returns a map with the given keys ks and the values from m accordingly.
  Setting nil for missing values in m."
  [m ks]
    ;; (let [ks (into #{} ks)]
    ;;   (into {} (filter #(ks (first %)) m)))
  (reduce #(assoc % %2 (get m %2)) {} ks))

(defn update-stats [configs]
  (let [{:keys [sprint-end-date sprint-length nr-sprints stats-file]} configs
        issues (load-update-issues configs)
        finished (finished-issues issues (t/offset-date-time 2020 11 1))
        sprints (sprints->csv (group-by-sprints
                               sprint-end-date nr-sprints sprint-length finished
                               {:ct (partial cycle-time-stats (:wip status-categories))
                                :throughput count
                                :status-times #(cleanup-map (reduce (fn [r status]
                                                                      (assoc r (first status)
                                                                             (/ (:avg (second status)) 60 24.0)))
                                                                    {} (status-stats %))
                                                            (:wip status-categories))}))
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
                     :status-times #(cleanup-map (reduce (fn [r status]
                                                           (assoc r (first status)
                                                                  (:sum (second status))))
                                                         {} (status-stats %))
                                                 (:wip status-categories))}))
