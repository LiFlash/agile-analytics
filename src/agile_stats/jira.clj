(ns agile-stats.jira
  (:require [agile-stats.configs :refer [base-url query-url status-categories]]
            [agile-stats.db
             :refer
             [histogram->csv persist-issues load-db read-issue-edn sprints->csv]]
            [agile-stats.issue
             :refer
             [finished-issues group-by-sprints update-issue-stats]]
            [agile-stats.metrics
             :refer
             [ct-histogram cycle-time-stats status-time-stats]]
            [agile-stats.utils :refer [cleanup-map vec->map]]
            [clj-http.client :as client]
            [clojure.data.csv :as csv]
            [java-time :as t]))

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

(defn get-issues [query & [update-date]]
  (let [issues (jira-paginate (str query-url
                                   query
                                   (and update-date (str " and status was not done before " (t/format "YYYY-MM-dd" update-date)))
                                   "&fields=created,status" ))]
    (->> issues
         (map parse-issue)
         (map (partial update-issue-stats status-categories)))))

(defn update-issue-db [configs]
  (let [{:keys [update-query update-date]} configs
        {:keys [issues last-update-date]} (load-db configs)
        change-date (if (:renew-db configs) update-date (or last-update-date update-date))
        issues (into (or issues {})
                     (-> update-query
                         (get-issues (when change-date (t/minus change-date (t/days 1))))
                         (vec->map :key)))]
    (persist-issues configs issues)
    (vals issues)))

;(def ds-issues (update-issue-db ds))
;(def cch-issues (update-issue-db cch))

(defn update-stats [configs]
  (let [{:keys [sprint-end-date sprint-length nr-sprints stats-file]} configs
        issues (update-issue-db configs)
        finished (finished-issues issues (t/offset-date-time 2020 11 1))
        sprints (sprints->csv (group-by-sprints
                               sprint-end-date nr-sprints sprint-length finished
                               {:ct (partial cycle-time-stats (:wip status-categories))
                                :throughput count
                                :status-times #(cleanup-map (reduce (fn [r status]
                                                                      (assoc r (first status)
                                                                             (/ (:avg (second status)) 60 24.0)))
                                                                    {} (status-time-stats %))
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
                                                         {} (status-time-stats %))
                                                 (:wip status-categories))}))
