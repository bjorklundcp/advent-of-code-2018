(ns advent-of-code-2018.day6
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn generate-coords
  [input]
  (->> input
       (map (fn [coord]
              (map (fn [string]
                     (Integer/parseInt string))
                   (str/split coord #", "))))
       (reduce (fn [[i m] coord]
                 [(inc i)
                  (assoc m i coord)])
               [0 {}])
       second))

(defn generate-bounds
  [coords]
  (->> coords
       (reduce-kv (fn [[max-x max-y] _ v]
                    [(max max-x (first v))
                     (max max-y (second v))])
                  [0 0])
       (map inc)))

(defn generate-grid
  [x-low x-high y-low y-high]
  (into #{} (for [x (range x-low (inc x-high)) y (range y-low (inc y-high))] (vector x y))))

(defn get-manhattan-distance
  [coord-a coord-b]
  (+ (Math/abs (- (first coord-a) (first coord-b)))
     (Math/abs (- (second coord-a) (second coord-b)))))

(defn get-closest-coord
  [coord target-coords]
  (let [target-coord-distances (->> target-coords
                                    (reduce-kv (fn [m k v]
                                                 (assoc m k (get-manhattan-distance coord v)))
                                               {})
                                    (sort-by val <))]
    (if (= (val (first target-coord-distances)) (val (second target-coord-distances)))
      {:x (first coord)
       :y (second coord)
       :closest -1}
      {:x (first coord)
       :y (second coord)
       :closest (key (first target-coord-distances))})))

(defn part-1
  [input]
  (let [coords (generate-coords (str/split-lines input))
        init-bounds (generate-bounds coords)]
    (loop [state {}
           stale-nodes #{}
           stable-nodes #{}
           x-low 0
           x-high (first init-bounds)
           y-low 0
           y-high (second init-bounds)]
      (if (= (count coords) (+ (count stale-nodes) (count stable-nodes)))
        (->> (reduce-kv (fn [m k v]
                          (if (> (:matched-n-times v) 10)
                            (assoc m k v)
                            m))
                        {}
                        state)
             (sort-by val #(> (:area %1) (:area %2)))
             first
             val
             :area)
        (let [new-areas (->> (generate-grid x-low x-high y-low y-high)
                             (map (fn [grid-coord]
                                    (get-closest-coord grid-coord coords)))
                             (remove #(= -1 (:closest %)))
                             (map :closest)
                             frequencies)
              new-state (reduce-kv (fn [m k v]
                                     (if (= (get-in state [k :area]) v)
                                       (->> (assoc {} :area v
                                                      :matched-n-times (inc (get-in state [k :matched-n-times]))
                                                      :failed-match-n-times 0)
                                            (assoc m k))
                                       (->> (assoc {} :area v
                                                      :matched-n-times 0
                                                      :failed-match-n-times (inc (get-in state [k :failed-match-n-times] 0)))
                                            (assoc m k))))
                                   {}
                                   new-areas)
              stale-nodes (reduce-kv (fn [m k v]
                                       (if (< 10 (:failed-match-n-times v))
                                         (conj m k)
                                         m))
                                     #{}
                                     new-state)
              stable-nodes (reduce-kv (fn [m k v]
                                        (if (< 10 (:matched-n-times v))
                                          (conj m k)
                                          m))
                                      #{}
                                      new-state)]
          (recur new-state
                 stale-nodes
                 stable-nodes
                 (dec x-low)
                 (inc x-high)
                 (dec y-low)
                 (inc y-high)))))))

(defn get-region-near-coords
  [coord target-coords n]
  (let [total-distance-to-coords (->> target-coords
                                      (reduce-kv (fn [m _ v]
                                                   (conj m (get-manhattan-distance coord v)))
                                                 [])
                                      (reduce +))]
    (if (> n total-distance-to-coords)
      {:x (first coord)
       :y (second coord)
       :distance total-distance-to-coords}
      {:x (first coord)
       :y (second coord)
       :distance -1})))

(defn part-2
  [input]
  (let [coords (generate-coords (str/split-lines input))
        init-bounds (generate-bounds coords)]
    (loop [area 0
           grid #{}
           n-times-equal 0
           x-low 0
           x-high (first init-bounds)
           y-low 0
           y-high (second init-bounds)]
      (if (= 10 n-times-equal)
        area
        (let [new-grid (-> (generate-grid x-low x-high y-low y-high)
                           (set/difference grid))
              new-area (->> new-grid
                            (pmap (fn [grid-coord]
                                    (get-region-near-coords grid-coord coords 10000)))
                            (remove #(= -1 (:distance %)))
                            count
                            (+ area))
              new-n-times-equal (if (and (not (= 0 new-area))
                                         (= new-area area))
                                  (inc n-times-equal)
                                  0)]
          (recur new-area
                 (set/union grid new-grid)
                 new-n-times-equal
                 (dec x-low)
                 (inc x-high)
                 (dec y-low)
                 (inc y-high)))))))
