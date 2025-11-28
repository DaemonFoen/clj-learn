(ns clj-learn.core-test
  (:require [clojure.test :refer :all]
            [clj-learn.second :refer :all]))

(deftest test-basic-integral
  (testing "sin(x) base"
    (let [F (integral #(Math/sin %) 0.0005)]
      (is (< (Math/abs (- (F Math/PI) 2.0)) 0.01))
      (is (< (Math/abs (- (F (/ Math/PI 2)) 1.0)) 0.01)))))

(deftest test-memo-integral-sin
  (testing "sin(x) memo"
    (let [F (integral-memo #(Math/sin %) 0.001)]
      (is (< (Math/abs (- (F Math/PI) 2.0)) 0.01))
      (is (< (Math/abs (- (F (/ Math/PI 2)) 1.0)) 0.01)))))

(deftest test-seq-integral-sin
  (testing "sin(x) seq"
    (let [F (integral-seq #(Math/sin %) 0.001)]
      (is (< (Math/abs (- (F Math/PI) 2.0)) 0.01))
      (is (< (Math/abs (- (F (/ Math/PI 2)) 1.0)) 0.01)))))

(deftest test-integral-linear
  (testing "f(x)=x -> x^2 / 2"
    (let [F (integral identity 0.0005)]
      (doseq [x [0 0.5 1 2 3 5]]
        (let [expected (/ (* x x) 2.0)]
          (is (< (Math/abs (- (F x) expected)) 0.01)))))))

(deftest test-integral-constant
  (testing "c = c*x"
    (let [F (integral (constantly 3) 0.001)]
      (doseq [x [0 1 2 5 10]]
        (is (< (Math/abs (- (F x) (* 3 x))) 0.01))))))

(deftest test-memo-integral
  (testing "Мемоизация работает"
    (let [counter (atom 0)
          f       (fn [x] (swap! counter inc) x)
          F       (integral-memo f 1.0)]

      (F 5)
      (let [calls-first @counter]

        (F 5)
        (is (= calls-first @counter))

        (F 10)
        (is (> @counter calls-first))))))

(deftest test-seq-integral
  (testing "Последовательность частичных интегралов"
    (let [F (integral-seq identity 1.0)]

      (is (= (F 0) 0.0))
      (is (= (F 1) 0.5))
      (is (= (F 2) 2.0))
      (is (= (F 3) 4.5)))))
