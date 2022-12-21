(ns advent-of-code-2022.day14
  (:require [advent-of-code-2022.core :as core]
            [clojure.string :as str]))

(def input
  (->> "day14.txt"
       core/read-input
       str/split-lines
       (map (fn [line] (map #(mapv core/parse-int (str/split % #","))
                            (str/split line #" -> "))))))

(def sample-input
  [[[498 4] [498 6] [496 6]]
   [[503 4] [502 4] [502 9] [494 9]]])

(defn abyss-row [scans]
  (reduce max (mapcat #(map second %) scans)))

(defn init-blocked [scans]
  (vec (repeat (inc (abyss-row scans)) #{})))

(defn mark-blocked [blocked [row col]]
  (update blocked row conj col))

(defn blocked? [blocked [row col]]
  ((get blocked row) col))

(def free? (complement blocked?))

(defn line->points [[col-a row-a] [col-b row-b]]
  (if (= row-a row-b)
    (map #(vector row-a %) (range (min col-a col-b) (inc (max col-a col-b))))
    (map #(vector % col-a) (range (min row-a row-b) (inc (max row-a row-b))))))

(defn scan->points [scan]
  (mapcat (fn [pair]
            (mapcat #(apply line->points %)
                    (partition 2 1 pair)))
          scan))

(defn scan->blocked [scan]
  (let [points (scan->points scan)]
    (reduce mark-blocked (init-blocked scan) points)))

(defn drop-sand [blocked [row col :as pos]]
  (let [down [(inc row) col]
        left [(inc row) (dec col)]
        right [(inc row) (inc col)]]
    (cond
      (>= (inc row) (count blocked)) [true blocked]
      (free? blocked down) (recur blocked down)
      (free? blocked left) (recur blocked left)
      (free? blocked right) (recur blocked right)
      :else [false (mark-blocked blocked pos)])))

(defn pour [blocked]
  (->> [false blocked]
       (iterate #(drop-sand (second %) [0 500]))
       (take-while (complement first))
       count
       dec))

(defn scan->floor-blocked [scan]
  (conj (scan->blocked scan) #{}))

(defn drop-floor-sand [blocked [row col :as pos]]
  (let [down [(inc row) col]
        left [(inc row) (dec col)]
        right [(inc row) (inc col)]]
   (cond
    (= (inc row) (count blocked)) [false (mark-blocked blocked pos)]
    (free? blocked down) (recur blocked down)
    (free? blocked left) (recur blocked left)
    (free? blocked right) (recur blocked right)
    (= pos [0 500]) [true (mark-blocked blocked pos)]
    :else [false (mark-blocked blocked pos)])))

(defn pour-floor [blocked]
  (->> [false blocked]
       (iterate #(drop-floor-sand (second %) [0 500]))
       (take-while (complement first))
       count))

(comment
  (pour (scan->blocked sample-input))
  (pour (scan->blocked input))
  
  (pour-floor (scan->floor-blocked sample-input))
  (pour-floor (scan->floor-blocked input))
  )