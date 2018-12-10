(ns advent-of-code-2018.day7
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(def alpha (->> (range (int \a) (inc (int \z)))
                (map #(str/upper-case (char %)))
                set))

(def alpha-ordered (->> (range (int \a) (inc (int \z)))
                        (mapv #(str/upper-case (char %)))))

(defn get-key-value
  [instruction]
  (->> (re-seq #"(?i)step ." instruction)
       (map (fn [a]
              (subs a (dec (count a)))))))

(defn get-available-steps
  [steps done-steps]
  (-> (reduce
        (fn [possible-steps [child parent]]
          (if (set/subset? parent done-steps)
            possible-steps
            (disj possible-steps child)))
        alpha
        steps)
      (set/difference done-steps)))

(defn get-next-step
  [steps done-steps]
  (-> (get-available-steps steps done-steps)
      sort
      first))

(defn walk-tree
  [tree]
  (loop [steps []]
    (if (= (count steps) (count alpha))
      (str/join steps)
      (recur (conj steps (get-next-step tree (set steps)))))))

(defn part-1
  [input]
  (->> (str/split-lines input)
       (reduce (fn [map instruction]
                 (let [[parent child] (get-key-value instruction)]
                   (update map child #(conj (or % #{}) parent))))
               {})
       walk-tree))

(defn get-time-for-instruction
  [instruction]
  (+ 60
     (inc (.indexOf alpha-ordered instruction))))

(defn reset-worker
  []
  {:instruction nil
   :total-time-on-job 0
   :current-time-on-job 0})

(defn worker-is-busy?
  [worker]
  (not (nil? (:instruction worker))))

(defn work-workers
  [workers completed-instructions]
  (reduce (fn [[workers completed-instructions] worker]
            (if (worker-is-busy? worker)
              (if (= (:total-time-on-job worker) (:current-time-on-job worker))
                [(conj workers (reset-worker)) (conj completed-instructions (:instruction worker))]
                [(conj workers {:instruction (:instruction worker)
                                :total-time-on-job (:total-time-on-job worker)
                                :current-time-on-job (inc (:current-time-on-job worker))})
                 completed-instructions])
              [(conj workers worker)
               completed-instructions]))
          [[] completed-instructions]
          workers))

(defn assign-workers
  [workers available-instructions]
  (if (every? worker-is-busy? workers)
    workers
    (let [currently-worked (->> (map :instruction workers)
                                (remove nil?)
                                set)]
      (-> (reduce (fn [[workers available-instructions] worker]
                    (if (worker-is-busy? worker)
                      [(conj workers worker)
                       available-instructions]
                      (if (< 0 (count available-instructions))
                        (let [instruction (-> available-instructions
                                              sort
                                              first)]
                          [(conj workers {:instruction instruction
                                          :total-time-on-job (get-time-for-instruction instruction)
                                          :current-time-on-job 1})
                           (disj available-instructions instruction)])
                        [(conj workers worker) available-instructions])))
                  [[] (set/difference available-instructions currently-worked)]
                  workers)
          first))))

(defn part-2
  [input]
  (let [instructions (->> (str/split-lines input)
                          (reduce (fn [map instruction]
                                    (let [[parent child] (get-key-value instruction)]
                                      (update map child #(conj (or % #{}) parent))))
                                  {}))]
    (loop [time -1
           completed-instructions #{}
           workers [(reset-worker)
                    (reset-worker)
                    (reset-worker)
                    (reset-worker)
                    (reset-worker)]]
      (if (and
            (= (count completed-instructions) (count alpha))
            (every? #(not (worker-is-busy? %)) workers))
        time
        (let [[workers completed-instructions] (work-workers workers completed-instructions)
              available-instructions (get-available-steps instructions completed-instructions)]
          (recur (inc time)
                 completed-instructions
                 (assign-workers workers available-instructions)))))))
