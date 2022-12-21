(ns advent-of-code-2022.day6
  (:require [advent-of-code-2022.core :as core]))

(def input (core/read-input "day6.txt"))

(core/defn-split start-of-data? [length | input]
  (apply distinct? (take length input)))

(defn suffixes [s]
  (if (seq s)
    (cons s (lazy-seq (suffixes (next s))))
    '(())))

(defn find-start-of-data [length input]
  (->> input
       suffixes
       (take-while (complement (start-of-data? length)))
       count
       (+ length)))

(comment
  (find-start-of-data 4 input)
  (find-start-of-data 4 "mjqjpqmgbljsphdztnvjfqwrcgsmlb")
  (find-start-of-data 4 "bvwbjplbgvbhsrlpgdmjqwftvncz")
  (find-start-of-data 4 "nppdvjthqldpwncqszvftbrmjlhg")
  (find-start-of-data 4 "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg")
  (find-start-of-data 4 "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw")
  
  (find-start-of-data 14 input)
  (find-start-of-data 14 "mjqjpqmgbljsphdztnvjfqwrcgsmlb")
  (find-start-of-data 14 "bvwbjplbgvbhsrlpgdmjqwftvncz")
  (find-start-of-data 14 "nppdvjthqldpwncqszvftbrmjlhg")
  (find-start-of-data 14 "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg")
  (find-start-of-data 14 "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw")
  )
