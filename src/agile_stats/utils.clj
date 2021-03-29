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
  "Assoc all items in v to the key returned by (key-fn item).
  Duplicate keys will be overwritten, so the last item found for a key will be mapped."
  [v key-fn]
  ;; TODO refactor to group-by plus removing the collection vals, so every key is mapped to a single item
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

(defn round
  "Round down a double to the given precision (number of significant digits)"
  [precision d]
  (let [factor (Math/pow 10 precision)]
    (/ (Math/floor (* d factor)) factor)))

(defn minutes->days [minutes]
  (round 1 (/ minutes 60 24.0)))
