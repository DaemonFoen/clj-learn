(ns clj-learn.third_test
  (:require [clojure.test :refer :all]
            [clj-learn.third :refer :all]))

(defn expensive-pred [n]
  (Thread/sleep 10)
  (even? n))

(defn gen-data [cnt]
  (vec (range cnt)))

(defn time-ms [f]
  (let [start (System/nanoTime)
        res (f)
        end (System/nanoTime)]
    {:result res
     :time-ms (/ (- end start) 1e6)}))

(deftest test-pfilter-correctness
  (let [data (gen-data 100)
        expected (filter even? data)
        actual (pfilter even? data 10)]
    (is (= expected actual))))

(deftest test-lazy-pfilter-correctness
  (let [data (range 100)
        expected (filter even? data)
        actual (take 50 (lazy-pfilter even? data 10))]
    (is (= (take 50 expected) actual))))

(deftest lazy-pfilter-is-lazy
  (let [counter (atom 0)
        pred (fn [x]
               (swap! counter inc)
               (odd? x))

        coll (range 100)
        batch-size 5

        result (lazy-pfilter pred coll batch-size)]

    (is (= 1 (first result)))
    (is (= @counter batch-size))

    (let [before @counter]
      (is (= [3 5] (take 2 (rest result))))
      (is (> @counter before)))))


(deftest test-performance
  (let [data (gen-data 200)
        filter-time (time-ms #(doall (filter expensive-pred data)))
        pfilter-time (time-ms #(pfilter expensive-pred data 20))
        lazy-pfilter-time (time-ms #(doall (take 100 (lazy-pfilter expensive-pred (range) 20 ))))]
    (println "filter:" (:time-ms filter-time) "ms")
    (println "pfilter:" (:time-ms pfilter-time) "ms")
    (println "lazy-pfilter:" (:time-ms lazy-pfilter-time) "ms")

    (is (= (filter expensive-pred data) (:result filter-time)))
    (is (= (pfilter expensive-pred data 20) (:result pfilter-time)))
    (is (= (take 100 (filter expensive-pred (range))) (:result lazy-pfilter-time)))))
