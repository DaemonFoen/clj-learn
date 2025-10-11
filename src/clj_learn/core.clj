(ns clj-learn.core)

; 1.1
(defn gen-strings [alphabet n]
  (if (= n 1)
    (map str alphabet)
    (for [prefix (gen-strings alphabet (dec n))
          c alphabet
          :when (not= (last prefix) c)]
      (str prefix c))))

; 1.2
(defn gen-strings-tail [alphabet n]
  (loop [res (map str alphabet)
         k 1]
    (if (= k n)
      res
      (recur
        (for [prefix res
              c alphabet
              :when (not= (last prefix) c)]
          (str prefix c))
        (inc k)))))


; 1.3
(defn my-map [f coll]
  (reduce (fn [acc x] (conj acc (f x))) [] coll))

(defn my-filter [pred coll]
  (reduce (fn [acc x]
            (if (pred x)
              (conj acc x)
              acc))
          [] coll))

; 1.4
(defn gen-strings-func [alphabet n]
  (reduce
    (fn [res _]
      (mapcat (fn [prefix]
                (filter (fn [s]
                          (not= (last prefix) (last s)))
                        (map #(str prefix %) alphabet)))
              res))
    (map str alphabet)
    (range 1 n)))
