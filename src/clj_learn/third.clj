(ns clj-learn.third)

(defn blocks-seq
  [n coll]
  (letfn [(step [s]
            (when (seq s)
              (let [b (take n s)]
                (cons b (lazy-seq (step (drop n s)))))))]
    (step coll)))

; 3.1
(defn pfilter
  [pred coll block]
  (let [blocks (doall (blocks-seq block coll))
        futures (mapv #(future (doall (filter pred %))) blocks)
        results (mapcat deref futures)]
    (into [] results)))

; 3.2
(defn lazy-pfilter
  [pred coll n]
  (letfn [(step [s]
            (lazy-seq
              (when-let [chunk (seq (take n s))]
                (let [rest   (drop n s)
                      pairs  (pmap (fn [x] [x (pred x)]) chunk)
                      passed (keep (fn [[x ok]] (when ok x)) pairs)]
                  (lazy-cat passed (step rest))))))]
    (step coll)))
