(ns advent-of-code-2022.day8
  (:require [advent-of-code-2022.core :as core]
            [clojure.string :as str]))

(def input
  (->> "day8.txt"
       core/read-input
       str/split-lines
       (mapv #(mapv core/parse-int %))))

(def sample-input [[3 0 3 7 3]
                   [2 5 5 1 2]
                   [6 5 3 3 2]
                   [3 3 5 4 9]
                   [3 5 3 9 0]])

(defn light-row [row]
  (loop [[curr & rest] row
         last-height -1
         accum []]
    (let [accum' (conj accum (< last-height curr))]
      (if rest
        (recur rest (max last-height curr) accum')
        accum'))))

(defn light-field [field]
  (map light-row field))

(defn run-cardinals [f field]
  [(f field)
   (->> field core/transpose f core/transpose)
   (->> field (map reverse) f (map reverse))
   (->> field core/transpose (map reverse) f (map reverse) core/transpose)])

(defn zip-fields [f fields]
  (reduce
   (fn zipmap [a b]
     (map (fn [row-a row-b] (map f row-a row-b)) a b))
   fields))

(defn count-lit [field]
  (reduce
   (fn [acc row] (+ acc (count (filter identity row))))
   0
   field))

(defn display-field [field]
  (->> field
       (map
        (fn [row]
          (apply str (map #(if % \* \-) row))))
       (str/join "\n")
       println))

(defn count-trees [stop-height row]
  (reduce (fn [acc tree] (if (< tree stop-height) (inc acc) (reduced (inc acc))))
          0 row))

(defn view-row [row]
  (loop [[curr & rest] row
         accum []]
    (let [accum' (conj accum (count-trees curr rest))]
      (if rest
        (recur rest accum')
        accum'))))

(defn view-field [field]
  (map view-row field))

(defn max-view [field]
  (reduce (fn [acc row] (max acc (reduce max row))) 0 field))

(comment
  (->> sample-input (run-cardinals light-field) (zip-fields #(or %1 %2)) count-lit)
  (->> sample-input (run-cardinals light-field) (zip-fields #(or %1 %2)) display-field)
  (->> input (run-cardinals light-field) (zip-fields #(or %1 %2)) count-lit)
  (->> input (run-cardinals light-field) (zip-fields #(or %1 %2)) display-field)

  (->> sample-input (run-cardinals view-field) (zip-fields *) max-view)
  (->> input (run-cardinals view-field) (zip-fields *) max-view)
  )