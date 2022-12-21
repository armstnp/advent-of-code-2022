(ns advent-of-code-2022.day5
  (:require [advent-of-code-2022.core :as core]
            [clojure.string :as str]
            [advent-of-code-2022.left-shark :as ls]))

(def input
  (->> "day5.txt"
       core/read-input
       str/split-lines))

(def sample-input (str/split-lines "    [D]    
[N] [C]    
[Z] [M] [P]
 1   2   3 

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2"))

(defn parse-crates [line]
  (->> line (drop 1) (take-nth 4)))

(defn parse-stacks [stack-lines]
  (->> stack-lines
       (drop-last 1)
       (map parse-crates)
       core/transpose
       (mapv #(drop-while (partial = \space) %))))

(def parse-move
  (comp (fn [m]
          (-> m
              (update :src dec)
              (update :dest dec)))
        :components
        (ls/parse
         (ls/! "move ")
         (ls/int! :quant)
         (ls/! " from ")
         (ls/int! :src)
         (ls/! " to ")
         (ls/int! :dest))))

(defn parse-input [lines]
  (let  [[stack-lines move-lines] (core/split-over seq lines)]
    {:stacks (parse-stacks stack-lines)
     :moves (map parse-move move-lines)}))

(defn run-move-9000 [{:keys [stacks moves]}]
  (let [[{:keys [quant src dest] :as move} & moves'] moves
        crate (first (get stacks src))
        move' (when (> quant 1) (update move :quant dec))]
    {:stacks (-> stacks
                 (update src next)
                 (update dest conj crate))
     :moves (if move' (conj moves' move') moves')}))

(defn run-moves [runner state]
  (->> state
       (iterate runner)
       (drop-while :moves)
       first))

(defn top-stacks [{:keys [stacks]}]
  (apply str (map first stacks)))

(comment
  (->> sample-input parse-input (run-moves run-move-9000) top-stacks)
  (->> input parse-input (run-moves run-move-9000) top-stacks)
  )

(defn run-move-9001 [{:keys [stacks moves] :as state}]
  (let [[{:keys [quant src dest]}] moves
        crates (take quant (get stacks src))]
    (-> state 
        (update-in [:stacks src] #(drop quant %))
        (update-in [:stacks dest] #(concat crates %))
        (update :moves next))))

(comment
  (->> sample-input parse-input (run-moves run-move-9001) top-stacks)
  (->> input parse-input (run-moves run-move-9001) top-stacks)
  )