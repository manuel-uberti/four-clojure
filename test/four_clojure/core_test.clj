(ns four-clojure.core-test
  (:require [clojure.test :refer :all]
            [four-clojure.core :refer :all]))

(deftest penultimate-element-test
  (testing "Testing penultimate-element function."
    (is (= (penultimate-element (list 1 2 3 4 5)) 4))
    (is (= (penultimate-element ["a" "b" "c"]) "b"))
    (is (= (penultimate-element [[1 2] [3 4]]) [1 2]))))

(deftest odd-numbers-test
  (testing "Testing odd-numbers function."
    (is (= (odd-numbers #{1 2 3 4 5}) '(1 3 5)))
    (is (= (odd-numbers [4 2 1 6]) '(1)))
    (is (= (odd-numbers [2 2 4 6]) '()))
    (is (= (odd-numbers [1 1 1 3]) '(1 1 1 3)))))

(deftest pack-sequence-test
  (testing "Testing pack-sequence function."
    (is (= (pack-sequence [1 1 2 1 1 1 3 3]) '((1 1) (2) (1 1 1) (3 3))))
    (is (= (pack-sequence [:a :a :b :b :c]) '((:a :a) (:b :b) (:c))))
    (is (= (pack-sequence [[1 2] [1 2] [3 4]]) '(([1 2] [1 2]) ([3 4]))))))

(deftest rotate-sequence-test
  (testing "Testing rotate-sequence function."
    (is (= (rotate-sequence 2 [1 2 3 4 5]) '(3 4 5 1 2)))
    (is (= (rotate-sequence -2 [1 2 3 4 5]) '(4 5 1 2 3)))
    (is (= (rotate-sequence 6 [1 2 3 4 5]) '(2 3 4 5 1)))
    (is (= (rotate-sequence 1 '(:a :b :c)) '(:b :c :a)))
    (is (= (rotate-sequence -4 '(:a :b :c)) '(:c :a :b)))))
