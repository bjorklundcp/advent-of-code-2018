(ns advent-of-code-2018.day5
  (:require [clojure.string :as str]))

(defn are-same-and-opposite-case?
  [char-a char-b]
  (and (= (str/upper-case char-a) (str/upper-case char-b))
       (or
         (and (Character/isUpperCase char-a) (Character/isLowerCase char-b))
         (and (Character/isLowerCase char-a) (Character/isUpperCase char-b)))))

(defn part-1
  [input]
  (loop [sequence (seq input)
         found-reaction? false
         current-sequence input
         index 0]
    (if (and (first sequence) (second sequence))
      (let [first-char (first sequence)
            second-char (second sequence)
            reaction? (are-same-and-opposite-case? first-char second-char)]
        (if reaction?
          (recur (nthnext sequence 2)
                 reaction?
                 (str
                   (subs current-sequence 0 index)
                   (subs current-sequence (+ 2 index)))
                 index)
          (recur (next sequence)
                 found-reaction?
                 current-sequence
                 (inc index))))
      (if found-reaction?
        (recur (seq current-sequence) false current-sequence 0)
        (count current-sequence)))))

(defn remove-all-letters
  [input letter-to-remove]
  (str/replace input (re-pattern (str "(?i)" letter-to-remove)) ""))

(defn part-2
  [input]
  (let [alphabet (seq "abcdefghijklmnopqrstuvwxyz")]
    (->> (pmap (fn [letter]
                 (-> (remove-all-letters input letter)
                     part-1))
               alphabet)
        (apply min))))
