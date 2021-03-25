(ns agile-stats.utils)

(defn select-vals
  "Returns the vals for keys ks ommitting nil values."
  [m ks]
  (reduce #(if-let [v (get m %2)]
             (conj % v) %) [] ks))

(defn merge-maps
  "Merges m2 into m1 using f as the merge function. f should accept the value to
  be updated from m1 and the appropriate value from m2 as arguments."
  [m1 m2 f]
  (reduce  #(update % (first %2) f (second %2)) m1 m2))

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

(defn apply-to-vals
  "Returns a map with mod-fn applied to vals."
  [f m]
  (reduce (fn [r entry]
            (assoc r (first entry)
                   (f (second entry))))
          {} m))
