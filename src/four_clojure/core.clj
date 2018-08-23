(ns four-clojure.core)

;; # 20
;; Penultimate element
;; Write a function which returns the second to last element from a sequence.
(defn penultimate-element
  [xs]
  (second (reverse xs)))

;; # 25
;; Find the odd numbers
;; Write a function which returns only the odd numbers from a sequence.
(defn odd-numbers
  [xs]
  (filter odd? xs))

;; # 31
;; Pack a Sequence
;; Write a function which packs consecutive duplicates into sub-lists.
(defn pack-sequence
  [xs]
  (partition-by identity xs))

;; # 43
;; Reverse Interleave
;; Write a function which reverses the interleave process into x number of
;; subsequences.
(defn reverse-interleave
  [xs x]
  (loop [counter (dec x)
         acc '()]
    (if (neg? counter)
      acc
      (recur (dec counter)
             (conj acc (map #(nth % counter) (partition x xs)))))))

;; # 44
;; Rotate Sequence
;; Write a function which can rotate a sequence in either direction.
(defn rotate-sequence
  [dir xs]
  (let [l (count xs)
        d (Math/abs dir)]
    (if (< d l)
      (let [step (if (neg? dir) (+ l dir) (inc (mod l d)))
            end (take step xs)
            start (drop step xs)]
        (concat start end))
      (let [step (if (neg? dir) (inc (- d l)) (- d l))
            end (take step xs)
            start (drop step xs)]
        (concat start end)))))

;; # 46
;; Flipping Out
;; Write a higher-order function which flips the order of the arguments of an
;; input function.
(defn flipping-out
  [op]
  (fn [x y]
    (op y x)))

;; # 50
;; Split By Type
;; Write a function which takes a sequence consisting of items with different
;; types and splits them up into a set of homogeneous sub-sequences. The
;; internal order of each sub-sequence should be maintained, but the
;; sub-sequences themselves can be returned in any order (this is why 'set' is
;; used in the test cases).
(defn split-by-type
  [xs]
  (reduce (fn [acc el]
            (->> xs
                 (filter #(= (type el) (type %)))
                 (into [])
                 (conj acc)))
          #{}
          xs))

;; # 55
;; Count Occurrences
;; Write a function which returns a map containing the number of occurences of
;; each distinct item in a sequence.
(defn count-occurrences
  [xs]
  (reduce (fn [acc el]
            (let [occurrences (count (filter #(= % el) xs))]
              (assoc acc el occurrences)))
          {}
          xs))

;; # 56
;; Find Distinct Items
;; Write a function which removes the duplicates from a sequence. Order of the
;; items must be maintained.
(defn distinct-items
  [xs]
  (reduce (fn [acc el]
            (if (some #(= % el) acc)
              acc
              (conj acc el)))
          []
          xs))

;; # 58
;; Function Composition
;; Write a function which allows you to create function compositions. The
;; parameter list should take a variable number of functions, and create a
;; function that applies them from right-to-left.
(defn fn-comp
  ([] identity)
  ([f] f)
  ([f g]
   (fn [& args]
     (f (apply g args))))
  ([f g & fs]
   (reduce fn-comp (conj fs g f))))

;; # 77
;; Anagram Finder
;; Write a function which finds all the anagrams in a vector of words. A word x
;; is an anagram of word y if all the letters in x can be rearranged in a
;; different order to form y. Your function should return a set of sets, where
;; each sub-set is a group of words which are anagrams of each other. Each
;; sub-set should have at least two words. Words without any anagrams should
;; not be included in the result.
(defn anagram?
  [s1 s2]
  (if (and s1 s2)
    (if (= (count s1) (count s2))
      (let [cs1 (seq (.toLowerCase s1))
            cs2 (seq (.toLowerCase s2))]
        (every? (set cs1) cs2))
      false)
    false))

(defn anagram-finder
  [xs]
  (->> xs
       (reduce (fn [acc el]
                 (if-let [match (set (filter #(anagram? % el) xs))]
                   (conj acc (conj match el))
                   (conj acc #{el})))
               #{})
       (filter #(> (count %) 1))
       set))

;; # 85
;; Power Set
;; Write a function which generates the power set of a given set. The power
;; set of a set x is the set of all subsets of x, including the empty set and x
;; itself.
(defn power-set
  [xs]
  (if (empty? xs)
    (conj #{} xs)
    (reduce (fn [acc el]
              (into acc (map #(conj % el) acc)))
            #{#{}}
            xs)))
