(ns agile-stats.jira
  (:require [agile-stats.configs :refer [status-categories]]
            [agile-stats.issue :refer [update-issue-stats]]
            [clj-http.client :as client]
            [java-time :as t]
            [clojure.data.csv :as csv]))

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
  (let [changelog (->> issue :key jira-changelog)]
;    (println (count changelog) ", ")
    (do {:self (str "https://digistore.atlassian.net/browse/" (:key issue))
         :key (:key issue)
         :summary (get-in issue [:fields :summary])
         :created-date (-> issue (get-in [:fields :created]) str->date)
         :created-status "To be defined"
         :status (get-in issue [:fields :status :name])
         :transitions (->> changelog transition-entries (sort-by :date t/before?))
         ;:orig-transitions changelog
         })))

(defn get-issues [query done-after-date update-date]
  ;;TODO make dates optional (in the signature)
  (let [format-date (partial t/format "YYYY-MM-dd")
        query (str query
                   (when done-after-date
                     (str " and status was not done before " (format-date done-after-date)))
                   (when update-date
                     (str " and updated >= " (format-date update-date)))
                   "&fields=created,status,summary" )
        _ (println "query: " query)
        issues (jira-paginate query)]
    (->> issues
         (map parse-issue)
         (map (partial update-issue-stats status-categories)))))

(comment [13
         10
         24
         7
         21
         25
         35
         11
         17
         36
         35
         24
         12
         3
         2
         1
         25
         5
         10
         8
         7
         7
         8
         7
         9
         25
         36
         45
         18
         37
         13
         13
         39
         8
         19
         39
         63
         22
         25
         39
         21
         28
         26
         15
         31
         27
         24
         91
         33
         13
         34
         32
         49
         108
         53
         15
         18
         35
         10
         20
         23
         53
         24
         33
         11
         21
         18
         32
         34
         15
         13
         39
         50
         12
         13
         13
         38
         36
         70
         12
         14
         15
         58
         30
         54
         14
         14
         32
         60
         31
         57
         18
         28
         51
         54
         11
         31
         28
         30
         46
         51
         36
         47
         57
         55
         69
         6
         12
         8
         9
         14
         19
         22
         18
         148
         45
         46
         39
         35
         59
         20
         26
         77
         61
         65
         28
         36
         49
         16
         45
         119
         24
         37
         16
         12
         23
         85
         18
         52
         51
         52
         56
         52
         56
         34
         28
         34
         31
         45
         10
         114
         48
         31
         29
         44
         48
         19
         23
         48
         11
         20
         30
         116
         17
         21
         16
         53
         105
         18
         16
         36
         89
         35
         17
         104
         44
         92
         51
         39
         16
         55
         11
         76
         20
         104
         74
         37
         35
         131
         38
         18
         23
         27
         32
         130
         41
         28
         66
         47
         75
         41
         33
         53
         44
         10
         74
         77
         123
         21
         16
         14
         93
         160
         42
         55
         132
         107
         42
         56
         29
         57
         62
         193
         53
         31
         82
         33
         58
         132
         65
         15
         39
         57
         5
         45
         7
         61
         32
         75
         18
         47
         85
         12
         28
         14
         102
         78
         8
         26
         40
         39
         51
         29
         25
         113
         91
         70
         242
         121
         56
         19
         92
         94
         80
         15
         66
         52
         176
         75
         39
         39
         65
         51
         74
         64
         97
         32
         50
         58
         59
         58
         82
         97
         77
         18
         75
         20
         53
         17
         61
         73
         52
         114
         103
         60
         112
         473
         105
         92
         49
         219
         96
         110
         31
         80
         21
         84
         95
         137
         33
         64
         47
         272
         20
         50
         103
         91
         130
         22
         22
         22
         159
         58
         74
         68
         61
         47
         33
         15
         17
         13
         17
         17
         21
         20
         22
         19
         21
         21
         20
         21
         23
         14
         19
         18
         21
         19
         17
         19
         18
         21
         20
         22
         19
         20
         20
         64
         22
         23
         69
         78
         10
         71
         38
         71
         97
         45
         25
         20
         52
         43
         51
         27
         6
         37
         61
         16
         183
         60
         59
         31
         41
         48
         33
         37
         39
         71
         56
         43
         68
         56
         54
         52
         39
         41
         83
         120
         109
         39
         52
         130
         58
         58
         179
         33
         38
         44
         57
         72
         178
         174
         63
         63
         82
         28
         65
         33
         32
         62
         31
         30
         33
         31
         54
         43
         65
         54
         105
         111
         101
         51
         53
         115
         49
         75
         72
         71
         54
         122
         59
         23
         19
         45
         20
         23
         25
         22
         24
         24
         28
         23
         25
         29
         65
         64
         26
         29
         19
         8])

(defn status-visits [issue]
  (let [hops (get-in issue [:transitions])]
    (map #(let [status (:to %)]
            (str (t/format "dd.MM.YY HH:mm: " (:date %)) status)) hops)))

(defn status-durations [issue]
  (map #(str (key %) ": " (agile-stats.utils/minutes->days (:duration (val %))))
       (filter #(and (contains? (:wip agile-stats.configs/status-categories) (key %))
                     (< 1 (agile-stats.utils/minutes->days (:duration (val %)))))
               (get-in issue [:stats :status-times]))))

(defn analyze-hops []
  (let [issues (->> (agile-stats.db/load-issues agile-stats.configs/ds)
                    vals
                    agile-stats.issue/finished-issues
                    (filter #(let [value  (get-in % [:stats :ct])
                                   days (if value
                                          (agile-stats.utils/minutes->days value) 0) ]
                               (and ;(> 35 days 20)
                                    (t/after? (:done-date %) (t/offset-date-time 2021 5 7)))))
                    ;(map :key)
                    )
        result (map #(-> [(:key %)
                          (:summary %)
                          (str "ct: " (agile-stats.utils/minutes->days (get-in % [:stats :ct])) 0)
                          (status-durations %)
                          (doall (status-visits %))])
                    issues)
        csv-result (reduce #(-> %
                                (conj (butlast (butlast %2)))
                                (conj (last (butlast %2)))
                                (into (map vector (last %2))))
                           [] result)]
    (with-open [writer (clojure.java.io/writer "analyze.csv")]
      (csv/write-csv writer csv-result))))

;; (defn transitions-past-100 []
;;   (let [issues  (->> (agile-stats.db/load-issues agile-stats.configs/ds)
;;                      vals
;;                      agile-stats.issue/finished-issues
;;                      (filter #(let [value  (get-in % [:orig-transitions])]
;;                                 (and (t/after? (:done-date %) (t/offset-date-time 2021 3 1))
;;                                      (< 100 (count value))))))]
;;     (count (map #(let [transitions (->> %
;;                                         :orig-transitions
;;                                         (drop 100)
;;                                         (filter transition?)
;;                                         (map parse-transition))]
;;                    (println "\n" (:key %) ", Summary: " (:summary %))
;;                    (println "ct: " (agile-stats.utils/minutes->days (get-in % [:stats :ct])) ", done: " (t/format (:done-date %)))
;;                    (println (map (fn [t] (str (:to t) ", ")) transitions)))
;;                issues))))
