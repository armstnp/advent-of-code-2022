(ns advent-of-code-2022.day13
  (:require [advent-of-code-2022.core :as core]
            [clojure.string :as str]))

(def input
  (->> "day13.txt"
       core/read-input
       str/split-lines
       (core/split-over (complement str/blank?))
       (mapv (fn [group] (mapv read-string group)))))

(def sample-input
  [[[1,1,3,1,1]
    [1,1,5,1,1]]

   [[[1],[2,3,4]]
    [[1],4]]

   [[9]
    [[8,7,6]]]

   [[[4,4],4,4]
    [[4,4],4,4,4]]

   [[7,7,7,7]
    [7,7,7]]

   [[]
    [3]]

   [[[[]]]
    [[]]]

   [[1,[2,[3,[4,[5,6,7]]]],8,9]
    [1,[2,[3,[4,[5,6,0]]]],8,9]]])

(defn order [fst snd]
  (cond
    (and (number? fst) (coll? snd)) (recur [fst] snd)
    (and (coll? fst) (number? snd)) (recur fst [snd])
    (and (number? fst) (number? snd)) (compare fst snd)
    (and (empty? fst) (empty? snd)) 0
    (and (seq fst) (empty? snd)) 1
    (and (empty? fst) (seq snd)) -1
    :else (let [o (order (first fst) (first snd))]
            (if (zero? o)
              (recur (rest fst) (rest snd))
              o))))

(comment
  (->> sample-input
       (map-indexed #(vector %1 (apply order %2)))
       (filter #(= -1 (second %)))
       (map (comp inc first))
       (reduce +))
  (->> input
       (map-indexed #(vector %1 (apply order %2)))
       (filter #(= -1 (second %)))
       (map (comp inc first))
       (reduce +))
  
  (->> sample-input
       (reduce into)
       (cons [[2]])
       (cons [[6]])
       (sort-by identity order)
       (map-indexed vector)
       (filter (comp #{[[2]] [[6]]} second))
       (map (comp inc first)) 
       (reduce *)) 
  (->> input
       (reduce into)
       (cons [[2]])
       (cons [[6]])
       (sort-by identity order)
       (map-indexed vector)
       (filter (comp #{[[2]] [[6]]} second))
       (map (comp inc first))
       (reduce *))
  )