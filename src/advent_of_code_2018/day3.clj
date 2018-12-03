(ns advent-of-code-2018.day3
  (:require [clojure.string :as str]))

(defn generate-plot-coords
  [coord area]
  (let [x (-> coord
              (str/split #",")
              first
              Integer.)
        y (-> coord
              (str/split #",")
              second
              Integer.)
        width (-> area
                  (str/split #"x")
                  first
                  Integer.)
        height (-> area
                   (str/split #"x")
                   second
                   Integer.)]
    (for [plot-x (range width)
          plot-y (range height)]
      [(+ x plot-x)
       (+ y plot-y)])))

(defn get-claimed-coords
  [input]
  (let [split-string (str/split input #" ")
        coord (str/join "" (drop-last (nth split-string 2)))
        area (nth split-string 3)]
    (generate-plot-coords coord area)))

(defn part-1
  [input]
  (->> input
       str/split-lines
       (mapcat get-claimed-coords)
       (reduce (fn [[known-coords dupe-coords] coord]
                 (if (and (contains? known-coords coord) (not (contains? dupe-coords coord)))
                   [(conj known-coords coord) (conj dupe-coords coord)]
                   [(conj known-coords coord) dupe-coords]))
               [#{} #{}])
       second
       count))

(defn format-input
  [map input]
  (let [split-string (str/split input #" ")
        coord (str/join "" (drop-last (nth split-string 2)))
        area (nth split-string 3)
        claimed-coords (generate-plot-coords coord area)]
    (assoc map
           (keyword (subs (first split-string) 1))
           claimed-coords)))

(defn part-2
  [input]
  (let [organized-input (->> input
                             str/split-lines
                             (reduce format-input {}))
        dupe-coords (->> input
                         str/split-lines
                         (mapcat get-claimed-coords)
                         (reduce (fn [[known-coords dupe-coords] coord]
                                   (if (and (contains? known-coords coord) (not (contains? dupe-coords coord)))
                                     [(conj known-coords coord) (conj dupe-coords coord)]
                                     [(conj known-coords coord) dupe-coords]))
                                 [#{} #{}])
                         second)]
    (reduce-kv (fn [id key value]
                 (if id
                   id
                   (if (contains-intersecting-coord? dupe-coords value)
                     nil
                     key)))
               nil
               organized-input)))