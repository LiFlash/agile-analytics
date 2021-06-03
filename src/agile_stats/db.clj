(ns agile-stats.db
  (:require [agile-stats.utils :refer [update-vals minutes->days]]
            [java-time :as t]))

;; (defn status-times->csv [sprints]
;;   (let [times (reduce (fn [r sprint]
;;                         (reduce #(update % (first %2) (fnil conj []) (second %2)) r (:status-times sprint)))
;;                       {} sprints)]
;;     (into [] (mapv #(into [(first %)] (second %)) times))))

(defn status-times->csv [sprints]
  (let [times (reduce (fn [r sprint]
                        (reduce #(update % (first %2) (fnil conj []) (second %2)) r (:status-times sprint)))
                      {} sprints)]
    (into [] (mapv #(into [(first %)] (second %)) times))))

(defn sprints->csv [sprints]
  (let [stats (reduce (fn [r sprint]
                        (let [stats (-> r
                                        (update :end-date conj (t/format "YYYY-MM-dd"(:end-date sprint)))
                                        (update :throughput conj (:throughput sprint))
                                        (update :avg conj (minutes->days (get-in sprint [:ct :avg])))
                                        (update :median conj (minutes->days (get-in sprint [:ct :median]))))]
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

(defn percentiles->csv
  ;;TODO rename/refactor to "map-to-rows"
  [percentiles]
  (let [ps (keys percentiles)
        vs (vals percentiles)]
    ;;TODO just return [ps vs] and not use conj
    (-> []
        (conj ps)
        (conj vs))))

(defn csv-transpose
  "Transposes two rows and applies the given formatter"
  [rows & [{:keys [format-first format-second]}]]
  ;;TODO Auf beliebig viele rows erweitern (die formatter)
  (let [t (apply mapv vector rows)]
    (vector (if format-first (mapv format-first (first t)) (first t))
            (if format-second (mapv format-second (second t)) (second t)))))

(defn status-ages->csv [ages]
  (let [age-fn #(minutes->days (get-in % [:stats :age]))
        row-fn (fn [issues]
                 (let [sorted (sort-by age-fn issues)]
                   (map #(str (:key %)
;                              ", " (:summary %)
                              ", "(age-fn %)) sorted)))]
    (into []
          (map #(into [(first %)] (row-fn (second %))) ages))))

;; (status-ages->csv {:status-one [{:key "one" :status :status-one :stats {:age 5}}]
;;                    :status-two [{:key "two" :stats {:age 7}}]})

(defn read-issue-edn [file-path]
  (when (.exists (clojure.java.io/file file-path))
    (clojure.edn/read-string
     {:readers {'object #(t/offset-date-time (last %))}}
     (slurp file-path))))

(defn load-db [configs]
  (read-issue-edn (:storage-file configs)))

(defn load-issues [configs]
  (:issues (load-db configs)))

(defn last-db-update [configs]
  (:last-update-date (load-db configs)))

(defn persist-db [configs db]
  (spit (:storage-file configs) (prn-str db))
  db)

(defn persist-issues [configs issues]
  (persist-db configs
              {:last-update-date (t/offset-date-time)
               :issues issues}))

(defn update-issues [configs update-fn]
  (let [db (load-db configs)
        issues (->> db
                   :issues
                   (update-vals update-fn))]
    (persist-db configs (assoc db :issues issues))))
