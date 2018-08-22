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

;; # 44
;; Rotate Sequence
;; Write a function which can rotate a sequence in either direction.
(defn rotate-sequence
  [dir xs]
  (let [l (count xs)
        d (Math/abs dir)]
    (if (< d l)
      (if (neg? dir)
        (let [step (+ l dir)
              end (take step xs)
              start (drop step xs)]
          (concat start end))
        (let [step (mod l d)
              end (take (inc step) xs)
              start (drop (inc step) xs)]
          (concat start end)))
      (let [step (- d l)
            end (if (neg? dir) (take (inc step) xs) (take step xs))
            start (if (neg? dir) (drop (inc step) xs) (drop step xs))]
        (concat start end)))))

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
