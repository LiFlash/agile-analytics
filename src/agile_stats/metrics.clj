(ns agile-stats.metrics
  (:require [agile-stats.issue :refer [status-times update-cycle-time status-hops cycle-time update-age]]
            [agile-stats.utils :refer [update-vals]]))

(defn avg [vs]
  (if (not (empty? vs))
    (/ (apply + vs) (count vs))
    0))

(defn median [ns]
  (if (not (empty? ns))
    (let [ns (sort ns)
          cnt (count ns)
          mid (bit-shift-right cnt 1)]
      (if (odd? cnt)
        (nth ns mid)
        (/ (+ (nth ns mid) (nth ns (dec mid))) 2)))
    0))

(defn stats [vs]
  {:avg (avg vs)
   :median (median vs)
   :sum (apply + vs)})

(defn cycle-time-stats
  "Avg. and Median cycle times over all issues. Arity one assumes the key [:stats :ct] exists for each issue"
  ([issues]
   (let [cycle-times (map #(get-in % [:stats :ct]) issues)]
     (stats cycle-times)))
  ([statuses issues]
   (let [issues (map #(update-cycle-time statuses %) issues)]
     (cycle-time-stats issues))))

;; (cycle-time-stats [:ip :review] [{:stats {:status-times {:ip {:duration 4}
;;                                                          :review {:duration 2}}}}
;;                                  {:stats {:status-times {:ip {:duration 5}
;;                                                          :review {:duration 11}}}}])

(defn status-time-stats
  "Returns a map with stats like avg, median and sum of time for each status the issues were in."
  [issues]
  (->> issues
       status-times
       (update-vals stats)))

(defn status-hop-stats
  [issues]
  (->> issues
       status-hops
       (update-vals stats)))
;; (status-time-stats [{:stats {:status-times {:ip {:duration 4}
;;                                             :review {:duration 2}}}}
;;                {:stats {:status-times {:ip {:duration 5}
;;                                        :review {:duration 11}}}}])

(defn ct-histogram [issues]
  (let [hist (group-by #(Math/round (/ (get-in % [:stats :ct]) 60 24.0)) issues)
        max-time (apply max (keys hist))
        times (range (inc max-time))]
    (reduce #(assoc % %2 (hist %2)) {} times)))

(defn percentile [p issues]
  (let [issue-count (count issues)]
    (->> issues
         (take (* issue-count p))
         last)))

(defn percentiles [issues]
  (let [sorted-issues (->> issues
                           (map #(get-in % [:stats :ct]))
                           sort)
        p-50 (percentile 0.5 sorted-issues)
        p-75 (percentile 0.75 sorted-issues)
        p-85 (percentile 0.85 sorted-issues)
        p-95 (percentile 0.95 sorted-issues)]
    {50 p-50
     75 p-75
     85 p-85
     95 p-95}))

(defn status-ages [statuses issues]
  (group-by :status (map (partial update-age statuses) issues)))
