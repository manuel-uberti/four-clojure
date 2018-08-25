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

(deftest split-by-type-test
  (testing "Testing split-by-type function."
    (is (= (set (split-by-type [1 :a 2 :b 3 :c])) #{[1 2 3] [:a :b :c]}))
    (is (= (set (split-by-type [:a "foo"  "bar" :b])) #{[:a :b] ["foo" "bar"]}))
    (is (= (set (split-by-type [[1 2] :a [3 4] 5 6 :b]))
           #{[[1 2] [3 4]] [:a :b] [5 6]}))))

(deftest partition-sequence-test
  (testing "Testing partition-sequence function."
    (is (= (partition-sequence 3 (range 9)) '((0 1 2) (3 4 5) (6 7 8))))
    (is (= (partition-sequence 2 (range 8)) '((0 1) (2 3) (4 5) (6 7))))
    (is (= (partition-sequence 3 (range 8)) '((0 1 2) (3 4 5))))))

(deftest count-occurrences-test
  (testing "Testing count-occurrences function."
    (is (= (count-occurrences [1 1 2 3 2 1 1]) {1 4, 2 2, 3 1}))
    (is (= (count-occurrences [:b :a :b :a :b]) {:a 2, :b 3}))
    (is (= (count-occurrences '([1 2] [1 3] [1 3])) {[1 2] 1, [1 3] 2}))))

(deftest distinct-items-test
  (testing "Testing distinct-items function."
    (is (= (distinct-items [1 2 1 3 1 2 4]) [1 2 3 4]))
    (is (= (distinct-items [:a :a :b :b :c :c]) [:a :b :c]))
    (is (= (distinct-items '([2 4] [1 2] [1 3] [1 3])) '([2 4] [1 2] [1 3])))
    (is (= (distinct-items (range 50)) (range 50)))))

(deftest fn-comp-test
  (testing "Testing fn-comp function."
    (is (= [3 2 1] ((fn-comp rest reverse) [1 2 3 4])))
    (is (= 5 ((fn-comp (partial + 3) second) [1 2 3 4])))
    (is (= true ((fn-comp zero? #(mod % 8) +) 3 5 7 9)))
    (is (= "HELLO"
           ((fn-comp #(.toUpperCase %) #(apply str %) take) 5 "hello world")))))

(deftest primes-test
  (testing "Testing primes function."
    (is (= (primes 2) [2 3]))
    (is (= (primes 5) [2 3 5 7 11]))
    (is (= (last (primes 100)) 541))))

(deftest word-sorting-test
  (testing "Testing word-sorting function."
    (is (= (word-sorting "Have a nice day.")
           ["a" "day" "Have" "nice"]))
    (is (= (word-sorting "Clojure is a fun language!")
           ["a" "Clojure" "fun" "is" "language"]))
    (is (= (word-sorting "Fools fall for foolish follies.")
           ["fall" "follies" "foolish" "Fools" "for"]))))

(deftest perfect-squares-test
  (testing "Testing perfect-squares function."
    (is (= (perfect-squares "4,5,6,7,8,9") "4,9"))
    (is (= (perfect-squares "15,16,25,36,37") "16,25,36"))))

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
