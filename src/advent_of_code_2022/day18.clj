(ns advent-of-code-2022.day18
  (:require [advent-of-code-2022.core :as core]
            [clojure.string :as str]
            [clojure.set :as cset]))

(def input
  (->> "day18.txt"
       core/read-input
       str/split-lines
       (map #(mapv core/parse-int (str/split % #",")))))

(def sample-input [[2,2,2] [1,2,2] [3,2,2] [2,1,2]
                   [2,3,2] [2,2,1] [2,2,3] [2,2,4]
                   [2,2,6] [1,2,5] [3,2,5] [2,1,5]
                   [2,3,5]])

(defn vadd [v1 v2]
  (mapv + v1 v2))

(defn adjacents [p]
  (mapv #(vadd % p)
        [[-1 0 0]
         [1 0 0]
         [0 -1 0]
         [0 1 0]
         [0 0 -1]
         [0 0 1]]))

(defn surface-area [scan]
  (let [scan (set scan)]
    (reduce
     +
     (map
      (fn [cube] (-> cube adjacents set
                     (cset/difference scan)
                     count))
      scan))))

(defn v> [v1 v2]
  (some #(apply > %) (map vector v1 v2)))

(defn flood [scan]
  (let [lower-bound (mapv (comp dec #(reduce min %)) (core/transpose scan))
        upper-bound (mapv (comp inc #(reduce max %)) (core/transpose scan))]
    (set
     (loop [filled #{lower-bound}
            queue [lower-bound]]
       (if (empty? queue)
         filled
         (let [[cube & queue'] queue
               adjs (filter #(not (or (filled %)
                                      (scan %)
                                      (v> lower-bound %)
                                      (v> % upper-bound)))
                            (adjacents cube))
               filled' (into filled adjs)]
           (recur filled' (concat queue' adjs))))))))

(defn exterior-surface-area [scan]
  (let [scan (set scan)
        exterior (flood scan)]
    (reduce
     +
     (map
      (fn [cube] (-> cube adjacents set
                     (cset/intersection exterior)
                     count))
      scan))))

(comment
  (surface-area sample-input)
  (surface-area input)
  
  (exterior-surface-area sample-input)
  (exterior-surface-area input)
  )