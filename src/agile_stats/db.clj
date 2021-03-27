(ns agile-stats.db
  (:require [agile-stats.utils :refer [update-vals]]
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

(defn percentiles->csv [percentiles]
  (let [ps (keys percentiles)
        vs (map #(/ % 60 24.0) (vals percentiles))]
    (-> []
        (conj ps)
        (conj vs))))

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
  (spit (:storage-file configs) (prn-str db)))

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
