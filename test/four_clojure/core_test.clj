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

(deftest reverse-interleave-test
  (testing "Testing reverse-interleave function."
    (is (= (reverse-interleave [1 2 3 4 5 6] 2) '((1 3 5) (2 4 6))))
    (is (= (reverse-interleave (range 9) 3) '((0 3 6) (1 4 7) (2 5 8))))
    (is (= (reverse-interleave (range 10) 5)
           '((0 5) (1 6) (2 7) (3 8) (4 9))))))

(deftest rotate-sequence-test
  (testing "Testing rotate-sequence function."
    (is (= (rotate-sequence 2 [1 2 3 4 5]) '(3 4 5 1 2)))
    (is (= (rotate-sequence -2 [1 2 3 4 5]) '(4 5 1 2 3)))
    (is (= (rotate-sequence 6 [1 2 3 4 5]) '(2 3 4 5 1)))
    (is (= (rotate-sequence 1 '(:a :b :c)) '(:b :c :a)))
    (is (= (rotate-sequence -4 '(:a :b :c)) '(:c :a :b)))))

(deftest flipping-out-test
  (testing "Testing flipping-out function."
    (is (= 3 ((flipping-out nth) 2 [1 2 3 4 5])))
    (is (= true ((flipping-out >) 7 8)))
    (is (= 4 ((flipping-out quot) 2 8)))
    (is (= [1 2 3] ((flipping-out take) [1 2 3 4 5] 3)))))

(deftest count-occurrences-test
  (testing "Testing count-occurrences function."
    (is (= (count-occurrences [1 1 2 3 2 1 1]) {1 4, 2 2, 3 1}))
    (is (= (count-occurrences [:b :a :b :a :b]) {:a 2, :b 3}))
    (is (= (count-occurrences '([1 2] [1 3] [1 3])) {[1 2] 1, [1 3] 2}))))

(deftest anagram-finder-test
  (testing "Testing anagram-finder function."
    (is (= (anagram-finder ["meat" "mat" "team" "mate" "eat"])
           #{#{"meat" "team" "mate"}}))
    (is (= (anagram-finder ["veer" "lake" "item" "kale" "mite" "ever"])
           #{#{"veer" "ever"} #{"lake" "kale"} #{"mite" "item"}}))))

(deftest power-set-test
  (testing "Testing power-set function."
    (is (= (power-set #{1 :a}) #{#{1 :a} #{:a} #{} #{1}}))
    (is (= (power-set #{}) #{#{}}))
    (is (= (power-set #{1 2 3})
           #{#{} #{1} #{2} #{3} #{1 2} #{1 3} #{2 3} #{1 2 3}}))
    (is (= (count (power-set (into #{} (range 10)))) 1024))))
