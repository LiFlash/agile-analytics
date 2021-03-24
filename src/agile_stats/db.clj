(ns agile-stats.db
  (:require [clojure.data.csv :as csv]
            [clj-http.client :as client]
            [java-time :as t]))

(defn status-times->csv [sprints]
  (let [times (reduce (fn [r sprint]
                        (reduce #(update % (first %2) (fnil conj []) (second %2)) r (:status-times sprint)))
                      {} sprints)]
    (into [] (mapv #(into [(first %)] (second %)) times))))
