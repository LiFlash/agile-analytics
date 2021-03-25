(ns agile-stats.metrics
  (:require [agile-stats.issue :refer [status-times update-cycle-time]]))

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

(defn status-time-stats
  "Returns a map with stats like avg, median and sum of time for each status the issues were in."
  [issues]
  (reduce (fn [r [status times]]
            (if (not (empty? times))
              (assoc r status (stats times))
              r))
          {} (status-times issues)))

;; (status-time-stats [{:stats {:status-times {:ip {:duration 4}
;;                                        :last-transition-date "Y"
;;                                        :review {:duration 2}}}}
;;                {:stats {:status-times {:ip {:duration 5}
;;                                        :last-transition-date "X"
;;                                        :review {:duration 11}}}}])

(defn ct-histogram [issues]
  (let [hist (group-by #(Math/round (/ (get-in % [:stats :ct]) 60 24.0)) issues)
        max-time (apply max (keys hist))
        times (range (inc max-time))]
    (reduce #(assoc % %2 (hist %2)) {} times)))

(defn status-hop-stats
  "Returns avg, median etc. for how often a state was reached per issue."
  [issues]
  )
