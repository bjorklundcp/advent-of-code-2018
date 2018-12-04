(ns advent-of-code-2018.day4
  (:require [clojure.string :as str]))

(defn get-guard-id
  [instruction]
  (->> instruction
       (re-find #"#\d+")
       (re-find #"\d+")
       Integer.))

(defn get-minute
  [instruction]
  (->> instruction
       (re-find #":\d+")
       (re-find #"\d+")
       Integer.))

(defn parse-instruction
  [[state id start] instruction]
  (cond
    (str/includes? instruction "begins shift")
    [state (get-guard-id instruction) nil]
    (str/includes? instruction "falls asleep")
    [state id (get-minute instruction)]
    (str/includes? instruction "wakes up")
    [(assoc state
            id
            (concat (get state id) (range start (get-minute instruction))))
     id
     nil]))

(defn part-1
  [input]
  (let [schedule (->> input
                      str/split-lines
                      sort
                      (reduce parse-instruction [{} nil nil])
                      first)
        guards-sleep-time (reduce-kv (fn [m k v]
                                       (assoc m k (count v)))
                                     {}
                                     schedule)
        most-asleep-guard (key (apply max-key val guards-sleep-time))
        most-slept-minute (->> (get schedule most-asleep-guard)
                               frequencies
                               (sort-by val)
                               last
                               key)]
    (* most-asleep-guard most-slept-minute)))

(defn part-2
  [input]
  (let [schedule (->> input
                      str/split-lines
                      sort
                      (reduce parse-instruction [{} nil nil])
                      first)
        guard-slept-minute-frequency (->> (reduce-kv (fn [m k v]
                                                       (assoc m k (->> (frequencies v)
                                                                       (sort-by val)
                                                                       reverse
                                                                       first)))
                                                     {}
                                                     schedule)
                                          (sort-by val #(> (second %1) (second %2)))
                                          first)]
    (* (key guard-slept-minute-frequency) (first (val guard-slept-minute-frequency)))))
