(ns advent-of-code-2022.day12
  (:require [advent-of-code-2022.core :as core]
            [clojure.string :as str]))

(def input
  (->> "day12.txt"
       core/read-input
       str/split-lines))

(def sample-input (str/split-lines "Sabqponm
abcryxxl
accszExk
acctuvwj
abdefghi"))

(defn find-ch [ch {:keys [maze width height]}]
  (reduce
   #(if (= (get-in maze %2) ch) (reduced %2) %1)
   nil
   (for [row (range height)
         col (range width)]
     [row col])))

(defn build-maze [lines]
  (let [protomaze {:maze lines
                   :height (count lines)
                   :width (count (first lines))
                   :visited #{}}
        protomaze (-> protomaze
                      (assoc :queue [[(find-ch \S protomaze) 0]])
                      (assoc :end (find-ch \E protomaze)))]
    (->> protomaze
         :queue first first hash-set
         (assoc protomaze :visited))))

(defn elevation [ch]
  (case ch
    \S (recur \a)
    \E (recur \z)
    (- (int ch) (int \a))))

(defn reachable [from to]
  (<= (elevation to) (inc (elevation from))))

(defn adjacents [{:keys [visited width height maze]} [row col :as from]]
  (->> [[row (inc col)] [row (dec col)] [(inc row) col] [(dec row) col]]
       (filterv (fn [[row col :as coord]]
                 (and (>= row 0) (< row height) (>= col 0) (< col width)
                      (not (visited coord))
                      (reachable (get-in maze from) (get-in maze coord)))))))

(defn step-bfs [{:keys [queue end] :as state}]
  (let [[cell depth] (first queue)
        adjs (adjacents state cell)
        queue' (into (subvec queue 1) (map #(vector % (inc depth)) adjs))]
    (if (= cell end)
      depth
      (-> state
          (assoc :queue queue')
          (update :visited into adjs)))))

(defn find-all-chs [chs {:keys [maze width height]}]
  (vec
   (for [row (range height)
         col (range width)
         :let [coord [row col]]
         :when (chs (get-in maze coord))]
     coord)))

(defn build-maze-for-as [lines]
  (let [protomaze {:maze lines
                   :height (count lines)
                   :width (count (first lines))
                   :visited #{}}
        starts (find-all-chs #{\S \a} protomaze)]
    (-> protomaze
        (assoc :queue (mapv #(vector % 0) starts))
        (assoc :end (find-ch \E protomaze))
        (assoc :visited (set starts)))))

(comment
  (core/iterate-to map? step-bfs (build-maze sample-input))
  (core/iterate-to map? step-bfs (build-maze input))
  
  (core/iterate-to map? step-bfs (build-maze-for-as sample-input))
  (core/iterate-to map? step-bfs (build-maze-for-as input))
  )