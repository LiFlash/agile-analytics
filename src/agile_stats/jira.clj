(ns agile-stats.jira
  (:require [agile-stats.configs :refer [status-categories]]
            [agile-stats.issue :refer [update-issue-stats]]
            [clj-http.client :as client]
            [java-time :as t]))

(def jql-query "search?jql=")

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
  (let [date (t/format "YYYY-MM-dd" update-date)
        issues (jira-paginate (str query
                                   (and date
                                        (str " and status was not done before " date " and updated > " date))
                                   "&fields=created,status" ))]
    (->> issues
         (map parse-issue)
         (map (partial update-issue-stats status-categories)))))
