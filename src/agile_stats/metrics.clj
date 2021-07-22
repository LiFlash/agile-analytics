(ns agile-stats.metrics
  (:require agile-stats.configs
            [agile-stats.issue
             :refer
             [status-durations
              status-visits
              status-hops
              status-times
              update-age
              update-cycle-time]]
            [agile-stats.utils :refer [update-vals select-vals]]
            [clojure.data.csv :as csv]
            [java-time :as t]))

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

;; (status-time-stats [{:stats {:status-times {:ip {:duration 4}
;;                                             :review {:duration 2}}}}
;;                {:stats {:status-times {:ip {:duration 5}
;;                                        :review {:duration 11}}}}])


(defn status-hop-stats
  [issues]
  (->> issues
       status-hops
       (update-vals stats)))

(defn ct-histogram [issues]
  (let [hist (group-by #(Math/round (/ (get-in % [:stats :ct]) 60 24.0)) issues)
        max-time (apply max (keys hist))
        times (range (inc max-time))]
    (reduce #(assoc % %2 (hist %2)) {} times)))

(defn percentile [p coll]
  (let [item-count (count coll)]
    (->> coll
         (take (* item-count p))
         last)))

(defn percentiles [fractions coll]
  (reduce #(assoc % %2 (percentile (/ %2 100) coll)) {} fractions))

(defn ct-percentiles [issues]
  (->> issues
       (map #(get-in % [:stats :ct]))
       sort
       (percentiles [50 75 85 95])))

(defn status-ages [statuses issues]
  (->> issues
       (map (partial update-age statuses))
       (group-by :status)))


(defn throughput-per-day
  "Returns a vector containing the throughput for each day between start and end date"
  [start end issues]
  (let [start (-> start
                  t/local-date)
        date-map (group-by #(when-let [done-date (:done-date %)]
                              (-> done-date t/local-date t/format)) issues)
        nr-of-days (-> start
                       (t/time-between end :days)
                       inc)]
    ;(println (count (sort-by first (update-vals count date-map))))
    ;; (doall (map #(let [date (->> % t/days (t/plus start) t/format)]
    ;;                (println date ": " (count (date-map date))))
    ;;             (range nr-of-days)))
    (map #(->> % t/days (t/plus start) t/format (date-map) count)
         (range nr-of-days))))

(defn- random-sum
  "Randomly picks n items from coll and sums them up. Key-fn can be supplied to
  select the val from each item."
  ([coll n] (random-sum coll n identity))
  ([coll n key-fn]
   (let [max-rand (count coll)]
     (->> (repeatedly n #(rand-int max-rand))
          (map #(->> %
                     (nth coll)
                     key-fn))
          (apply +)))))

(defn monte-carlo-issues
  [days reference-throughput]
  (let [iterations (for [i (range 30000)]
                     (random-sum reference-throughput days))]
    (->> iterations
         sort
         reverse
         (percentiles [50 75 85 95]))))

;(monte-carlo-issues 14 (repeatedly 60 #(rand-int 3)))

(defn- days-until-release
  [nr-issues reference-throughput]
  (let [max-rand (count reference-throughput)]
    (loop [nr-issues nr-issues
           nr-days 1]
      (let [throughput (nth reference-throughput (rand-int max-rand))
            left-issues (- nr-issues throughput)]
        (if (<= left-issues 0)
          nr-days
          (recur left-issues (inc nr-days)))))))

(defn monte-carlo-time [nr-issues start-date reference-throughput]
  (let [iterations (for [i (range 30000)]
                     (days-until-release nr-issues reference-throughput))

        p (->> iterations
               sort
               ;reverse
               (percentiles [50 75 85 95]))]
    ;;TODO convert percentiles to dates instead of strings (strings are used to take the formatting away from the caller)
    (update-vals #(->> %
                      t/days
                      (t/plus start-date)
                      (t/format "YYYY-MM-dd"))
                 p)))

;(monte-carlo-time 10 (t/offset-date-time) [5 1 1 3 2 1 2 5 3])

(defn detailed-hops [statuses issues]
  (map #(-> {:key (:key %)
             :summary (:summary %)
             :ct (str "ct: " (agile-stats.utils/minutes->days (get-in % [:stats :ct])) 0)
             :estimate (str "estimate: " (:estimate %))
             :durations (status-durations statuses %)
             :hops (status-visits %)})
       issues))
