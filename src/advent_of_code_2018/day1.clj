(ns advent-of-code-2018.day1
  (:require [clojure.string :as str]))

(defn make-number
  [input]
  (let [modifier (subs input 0 1)
        value (Integer. (subs input 1))]
    (case modifier
      "+" value
      "-" (* -1 value))))

(defn part-1
  [input]
  (->> input
       str/split-lines
       (map make-number)
       (reduce +)))

(defn part-2
  [input]
  (let [init-frequencies (map make-number (str/split-lines input))]
    (loop [frequencies init-frequencies
           frequencies-found [0]
           current-frequency 0]
      (if frequencies
        (let [new-frequency (+ current-frequency (first frequencies))]
          (if (some #{new-frequency} frequencies-found)
            new-frequency
            (recur (next frequencies) (conj frequencies-found new-frequency) new-frequency)))
        (recur init-frequencies frequencies-found current-frequency)))))
