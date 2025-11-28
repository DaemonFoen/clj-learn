(ns clj-learn.second)

; 2
(defn integral
  [f h]
  (fn [x]
    (let [n  (long (/ x h))
          xn (* n h)
          sum (reduce + (map f (range 0 xn h)))]
      (+ (* h sum)
         (* 0.5 h (- (f xn) (f 0)))))))

; 2.1
(defn integral-memo
  [f h]
  (let [F-basic (integral f h)]
    (memoize F-basic)))

; 2.2
(defn partial-integrals
  [f h]
  (letfn [(step [prev-x prev-I]
            (let [x (+ prev-x h)
                  new-I (+ prev-I (* 0.5 h (+ (f prev-x) (f x))))]
              (lazy-seq (cons new-I (step x new-I)))))]
    (lazy-seq
      (cons 0.0 (step 0.0 0.0)))))


(defn integral-seq
  [f h]
  (let [ints (partial-integrals f h)]
    (fn [x]
      (let [k (long (/ x h))]
        (nth ints k)))))
