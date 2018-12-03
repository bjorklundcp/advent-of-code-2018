(ns advent-of-code-2018.day2
  (:require [clojure.string :as str]))

(defn generate-occurances
  [occurances frequencies]
  (let [two-occurances (if (some #{2} (vals frequencies))
                         (inc (first occurances))
                         (first occurances))
        three-occurances (if (some #{3} (vals frequencies))
                           (inc (second occurances))
                           (second occurances))]
    [two-occurances three-occurances]))

(defn part-1
  [input]
  (let [occurances (->> input
                        str/split-lines
                        (map frequencies)
                        (reduce generate-occurances [0 0]))]
    (* (first occurances) (second occurances))))

(defn off-by-one?
  [string-a string-b]
  (loop [chars-a (seq string-a)
         chars-b (seq string-b)
         num-differences 0]
    (if (< 1 num-differences)
      false
      (if chars-a
        (if (= (first chars-a) (first chars-b))
          (recur (next chars-a) (next chars-b) num-differences)
          (recur (next chars-a) (next chars-b) (inc num-differences)))
        (= num-differences 1)))))

(defn generate-answer
  [string-a string-b]
  (loop [chars-a (seq string-a)
         chars-b (seq string-b)
         answer []]
    (if chars-a
      (if (= (first chars-a) (first chars-b))
        (recur (next chars-a) (next chars-b) (conj answer (first chars-a)))
        (recur (next chars-a) (next chars-b) answer))
      (apply str answer))))

(defn get-answer-if-exists
  [id all-ids]
  (loop [ids all-ids]
    (when (first ids)
      (if (off-by-one? id (first ids))
        (generate-answer id (first ids))
        (recur (next ids))))))

(defn part-2
  [input]
  (let [all-ids (str/split-lines input)]
    (loop [ids all-ids]
      (if-let [answer (get-answer-if-exists (first ids) all-ids)]
        answer
        (recur (next ids))))))
