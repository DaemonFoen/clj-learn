(ns clj-learn.four_test
  (:require [clojure.test :refer :all]
            [clj-learn.four :refer :all]))

(deftest substitute-simple-variable
  (testing
    (is (= (const true)
           (substitute (v "x") "x" true)))))

(deftest substitute-other-var-untouched
  (testing
    (is (= (v "y")
           (substitute (v "y") "x" true)))))

(deftest substitute-deep
  (testing
    (let [expr (and* (v "x") (or* (v "y") (not* (v "x"))))]
      (is (= (and* (const false)
                   (or* (v "y") (not* (const false))))
             (substitute expr "x" false))))))

(deftest simplify-double-negation
  (testing "Упрощение ¬(¬A)"
    (is (= (v "a")
           (simplify (not* (not* (v "a"))))))))

(deftest simplify-and-constants
  (testing "Упрощение AND с константами"
    (is (= (const false)
           (simplify (and* (v "x") (const false)))))
    (is (= (v "x")
           (simplify (and* (v "x") (const true)))))))

(deftest simplify-or-constants
  (testing "Упрощение OR с константами"
    (is (= (const true)
           (simplify (or* (v "x") (const true)))))
    (is (= (v "x")
           (simplify (or* (v "x") (const false)))))))

(deftest impl-expansion
  (testing "a → b преобразуется в ¬a ∨ b"
    (is (= (or* (not* (v "a")) (v "b"))
           (simplify (impl (v "a") (v "b")))))))

(deftest dnf-distribute
  (testing "x ∧ (y ∨ z)  → (x ∧ y) ∨ (x ∧ z)"
    (let [expr (and* (v "x") (or* (v "y") (v "z")))]
      (is (= (or* (and* (v "x") (v "y"))
                  (and* (v "x") (v "z")))
             (to-dnf expr))))))

(deftest dnf-negation-over-or
  (testing "¬(x ∨ y)  →  ¬x ∧ ¬y"
    (let [expr (not* (or* (v "x") (v "y")))]
      (is (= (and* (not* (v "x")) (not* (v "y")))
             (to-dnf expr))))))

(deftest dnf-negation-over-and
  (testing "¬(x ∧ y)  →  ¬x ∨ ¬y"
    (let [expr (not* (and* (v "x") (v "y")))]
      (is (= (or* (not* (v "x"))
                  (not* (v "y")))
             (to-dnf expr))))))

(deftest dnf-complex
  (testing "(x ∧ (y ∨ ¬z)) ∨ ¬(y ∧ z)"
    (let [expr (or*
                 (and* (v "x")
                       (or* (v "y") (not* (v "z"))))
                 (not* (and* (v "y") (v "z"))))
          actual (to-dnf expr)

          expected
          (or*
            (or*
              (and* (v "x") (v "y"))
              (and* (v "x") (not* (v "z"))))
            (or*
              (not* (v "y"))
              (not* (v "z"))))]
      (is (= expected actual)))))
