(ns advent-of-code-2022.day20
  (:require [advent-of-code-2022.core :as core]
            [clojure.string :as str]))

(def input
  (->> "day20.txt"
       core/read-input
       str/split-lines
       (mapv core/parse-int)))

(def sample-input [1 2 -3 3 -2 0 4])

(defn center
  "Rotates coll so the first occurrence of x is at the fore"
  [x coll]
  (->> coll
       (split-with #(not= % x))
       (apply #(concat %2 %1))))

(defn mix [times mixer]
  (map
   #(nth mixer %)
   (loop [indices (range (count mixer))
          curr 0]
     (if (>= curr (* times (count mixer)))
       indices
       (let [flat-cur (mod curr (count mixer))
             n (nth mixer flat-cur)
             shift (mod n (dec (count mixer)))
             centered (center flat-cur indices)
             shifted (->> centered rest
                          (split-at shift)
                          (apply #(concat %2 %1))
                          (cons flat-cur))]
         (recur shifted (inc curr)))))))

(comment
  (let [mixed (->> sample-input (mix 1) (center 0) cycle)]
    (+ (nth mixed 1000) (nth mixed 2000) (nth mixed 3000)))
  
  (let [mixed (->> input (mix 1) (center 0) cycle)]
    (+ (nth mixed 1000) (nth mixed 2000) (nth mixed 3000)))
  
  (let [mixed (->> sample-input (map #(* 811589153 %)) (mix 10) (center 0) cycle)]
    (+ (nth mixed 1000) (nth mixed 2000) (nth mixed 3000)))
  
  (let [mixed (->> input (map #(* 811589153 %)) (mix 10) (center 0) cycle)]
    (+ (nth mixed 1000) (nth mixed 2000) (nth mixed 3000)))

  )