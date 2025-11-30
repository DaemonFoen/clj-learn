(ns clj-learn.four)

(defn v [name] {:type :var :name name})
(defn const [v] {:type :const :value v})
(defn not* [e] {:type :not :arg e})
(defn and* [& xs] {:type :and :args xs})
(defn or*  [& xs] {:type :or  :args xs})
(defn impl [a b] {:type :impl :args [a b]})

(defn substitute
  [expr x v]
  (case (:type expr)
    :var   (if (= (:name expr) x) (const v) expr)
    :const expr
    :not   (not* (substitute (:arg expr) x v))
    :and   (apply and* (map #(substitute % x v) (:args expr)))
    :or    (apply or*  (map #(substitute % x v) (:args expr)))
    :impl  (let [[a b] (:args expr)]
             (impl (substitute a x v) (substitute b x v)))))


(defn simplify [expr]
  (case (:type expr)
    :const expr
    :var expr
    :not
    (let [a (simplify (:arg expr))]
      (case (:type a)
        :const (const (not (:value a)))
        :not (:arg a)
        (not* a)))
    :and
    (let [xs (map simplify (:args expr))]
      (if (some #(and (= (:type %) :const) (false? (:value %))) xs)
        (const false)
        (let [xs (remove #(and (= (:type %) :const) (true? (:value %))) xs)]
          (cond
            (empty? xs) (const true)
            (= 1 (count xs)) (first xs)
            :else (apply and* xs)))))
    :or
    (let [xs (map simplify (:args expr))]
      (if (some #(and (= (:type %) :const) (true? (:value %))) xs)
        (const true)
        (let [xs (remove #(and (= (:type %) :const) (false? (:value %))) xs)]
          (cond
            (empty? xs) (const false)
            (= 1 (count xs)) (first xs)
            :else (apply or* xs)))))
    :impl
    (let [[a b] (:args expr)]
      ;; A → B  === ¬A ∨ B
      (simplify (or* (not* a) b)))))


(defn distribute
  "X ∧ (A ∨ B) => (X ∧ A) ∨ (X ∧ B)"
  [a b]
  (cond
    (and (= (:type a) :or)) (apply or* (map #(distribute % b) (:args a)))
    (and (= (:type b) :or)) (apply or* (map #(distribute a %) (:args b)))
    :else (and* a b)))


(defn to-dnf [expr]
  (let [e (simplify expr)]
    (case (:type e)
      (:var :const) e
      :not
      (let [a (to-dnf (:arg e))]
        (case (:type a)
          :var e
          :const (const (not (:value a)))
          :and (apply or* (map #(to-dnf (not* %)) (:args a)))
          :or  (apply and* (map #(to-dnf (not* %)) (:args a)))
          e))
      :and
      (reduce distribute (map to-dnf (:args e)))
      :or
      (apply or* (map to-dnf (:args e))))))
