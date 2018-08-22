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
    (cond
      (< d l)
      (if (neg? dir)
        (let [step (+ (count xs) dir)
              end (take step xs)
              start (drop step xs)]
          (concat start end))
        (let [step (mod l d)
              end (take (inc step) xs)
              start (drop (inc step) xs)]
          (concat start end)))

      (= d l)
      xs

      :else
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
