(ns agile-stats.utils)

(defn select-vals
  "Returns the vals for keys ks ommitting nil values."
  [m ks]
  (reduce #(if-let [v (get m %2)]
             (conj % v) %) [] ks))

(defn merge-maps
  "Merges an arbitrary number of maps using f as the merge function.
  f should accept two arguments. The value to be updated in the result
  map (m1 in the beginning) and the appropriate value from the next map as arguments."
  ([f m] m)
  ([f m1 m2]
   (reduce #(update % (first %2) f (second %2)) m1 m2))
  ([f m1 m2 m3 & ms]
   (reduce (partial merge-maps f) m1 (into [m2 m3] ms))))

(defn vec->map
  "Assoc all items in v to the key returned by (key-fn item)"
  [v key-fn]
  (reduce #(assoc % (key-fn %2) %2) {} v))

(defn cleanup-map
  "Like select-keys but sets nil for missing values."
  [ks m]
    ;; (let [ks (into #{} ks)]
    ;;   (into {} (filter #(ks (first %)) m)))
  (reduce #(assoc % %2 (get m %2)) {} ks))

(defn update-vals
  "Returns a map with mod-fn applied to vals."
  [f m]
  (reduce (fn [r entry]
            (assoc r (first entry)
                   (f (second entry))))
          {} m))
