(ns agile-stats.jira
  (:require [agile-stats.configs :refer [status-categories]]
            [agile-stats.issue :refer [update-issue-stats]]
            [agile-stats.utils :refer [vec->map]]
            [clj-http.client :as client]
            [java-time :as t]))

(def jql-query "/rest/api/2/search?jql=")

(def issue-fields "&fields=created,status,summary,timetracking")

(defn issue-query [base-url query]
  (str base-url jql-query query issue-fields))

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
  (let [ ;changelog (->> issue :key jira-changelog)
        ]
;    (println (count changelog) ", ")
    (do {:self (str "https://digistore.atlassian.net/browse/" (:key issue))
         :key (:key issue)
         :issuetype (get-in issue [:fields :issuetype :name])
         :summary (get-in issue [:fields :summary])
         :created-date (-> issue (get-in [:fields :created]) str->date)
         :created-status "To be defined"
         :status (get-in issue [:fields :status :name])
         ;:transitions (->> changelog transition-entries (sort-by :date t/before?))
         :estimate (get-in issue [:fields :timetracking :originalEstimate])
         ;:orig-transitions changelog
         })))

(defn parse-issues [issues]
  (map parse-issue issues))

(defn get-transitions
  "Loads the changelog from jira, extracts the tranisitions for each issue and adds them in :transitions"
  [issues]
  (map #(assoc % :transitions (->> %
                                   :key
                                   jira-changelog
                                   transition-entries
                                   (sort-by :date t/before?)))
       issues))

(defn update-details [base-url issues]
  (let [issue-map (vec->map issues :key)
        ks-str (->> issue-map
                    keys
                    (reduce #(str % %2 ",") "")
                    butlast
                    (apply str))
        query (issue-query base-url (str "issuekey in (" ks-str ")"))
        new-issues (-> query
                       jira-paginate
                       parse-issues
                       (vec->map :key))]
    (->> new-issues
         (merge-with into issue-map)
         vals)))

(defn get-issues [base-url query & [done-after-date update-date]]
  (let [format-date (partial t/format "YYYY-MM-dd")
        query (issue-query base-url (str query
                                        (when done-after-date
                                          (str " and status was not done before " (format-date done-after-date)))
                                        (when update-date
                                          (str " and updated >= " (format-date update-date)))))
        _ (println "query: " query)
        issues (jira-paginate query)]
    (->> issues
         parse-issues
         get-transitions)))
