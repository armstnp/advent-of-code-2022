(ns advent-of-code-2022.day10
  (:require [advent-of-code-2022.core :as core]
            [clojure.string :as str]
            [advent-of-code-2022.left-shark :as ls]))

(def input
  (->> "day10.txt" core/read-input str/split-lines))

(def sample-input-1 ["noop" "addx 3" "addx -5"])

(def sample-input-2
  (->> "day10-sample.txt" core/read-input str/split-lines))

(def parse-addx
  (comp #(vector :addx (:value %))
        :components
        (ls/parse
         (ls/! "addx ")
         (ls/int! :value))))

(defn parse-line [line]
  (if (= line "noop")
    [:noop]
    (parse-addx line)))

(defmulti run-line (comp first first :instructions))

(defmethod run-line :noop
  [{:keys [x] :as state}]
  [[x]
   (update state :instructions next)])

(defmethod run-line :addx
  [{:keys [x instructions] :as state}]
  (let [[[_ value] & instructions'] instructions]
    [[x x]
     (-> state
         (assoc :instructions instructions')
         (update :x + value))]))

(defn init-machine [instructions]
  {:instructions instructions
   :x 1})

(defn run-machine [machine]
  (loop [state machine
         acc []]
    (if-not (:instructions state)
      acc
      (let [[xs state'] (run-line state)]
        (recur state' (concat acc xs))))))

(defn sprite-char [pixel sprite-loc]
  (if (#{sprite-loc (dec sprite-loc) (inc sprite-loc)} pixel)
    \# \space))

(comment
  (->> sample-input-1 (map parse-line) init-machine run-machine)
  (->> sample-input-2 (map parse-line) init-machine run-machine
       (drop 19) (take-nth 40)
       (map * (map #(+ 20 (* 40 %)) (range)))
       (reduce +)) 
  (->> input (map parse-line) init-machine run-machine
       (drop 19) (take-nth 40) (take 6)
       (map * (map #(+ 20 (* 40 %)) (range)))
       (reduce +))
  
  (->> sample-input-2 (map parse-line) init-machine run-machine
       (partition 40)
       (map #(map-indexed sprite-char %))
       (map #(apply str %))
       (map println)
       doall) 
  (->> input (map parse-line) init-machine run-machine
       (partition 40)
       (map #(map-indexed sprite-char %))
       (map #(apply str %))
       (map println)
       doall)
  )
