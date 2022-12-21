(ns advent-of-code-2022.day9
  (:require [advent-of-code-2022.core :as core]
            [clojure.string :as str]))

(def input
  (->> "day9.txt"
       core/read-input
       str/split-lines
       (map (fn [x]
              (let [[dir steps] (str/split x #" ")]
                [(keyword dir) (core/parse-int steps)])))))

(def sample-input '([:R 4] [:U 4] [:L 3] [:D 1] [:R 4] [:D 1] [:L 5] [:R 2]))

(def sample-input-2 '([:R 5] [:U 8] [:L 8] [:D 3] [:R 17] [:D 10] [:L 25] [:U 20]))

(defn pop-move [moves]
  (let [[[move steps] & moves'] moves
        steps' (dec steps)
        moves' (if (zero? steps') moves' (cons [move steps'] moves'))]
    [move moves']))

(def vadd (partial mapv +))
(def vneg (partial mapv -))

(def head-move
  {:U [0 -1]
   :L [-1 0]
   :D [0 1]
   :R [1 0]})

(defn rope-pull
  "Gives the offset for the tail, and the inverse offset for the head-offset."
  [head-offset]
  (case head-offset
    [-2 0] [-1 0]
    [0 -2] [0 -1]
    [2 0] [1 0]
    [0 2] [0 1]
    ([-2 2] [-2 1] [-1 2]) [-1 1]
    ([-2 -2] [-2 -1] [-1 -2]) [-1 -1]
    ([2 -2] [2 -1] [1 -2]) [1 -1]
    ([2 2] [2 1] [1 2]) [1 1]
    [0 0]))

(defn init-rope [moves]
  {:tail-pos [0 0]
   :visited #{[0 0]}
   :head-offset [0 0]
   :moves moves})

(defn run-move [{:keys [tail-pos visited head-offset moves]}]
  (let [[move moves'] (pop-move moves)
        head-offset' (vadd head-offset (head-move move))
        tail-offset (rope-pull head-offset')
        tail-pos' (vadd tail-offset tail-pos)
        head-offset' (vadd head-offset' (vneg tail-offset))
        visited' (conj visited tail-pos')]
    {:tail-pos tail-pos'
     :visited visited'
     :head-offset head-offset'
     :moves moves'}))

(defn chain-pull [offsets move]
  (loop [acc []
         [offset & offsets'] offsets
         move (head-move move)]
    (let [offset' (vadd offset move)
          move' (rope-pull offset')
          offset' (vadd offset' (vneg move'))
          acc' (conj acc offset')]
      (if (or (not (seq offsets'))
              (= [0 0] move'))
        [move' (concat acc' offsets')]
        (recur acc' offsets' move')))))

(defn init-chain [moves]
  {:tail-pos [0 0]
   :visited #{[0 0]}
   :offsets (repeat 9 [0 0])
   :moves moves})

(defn run-chain-move [{:keys [tail-pos visited offsets moves]}]
  (let [[move moves'] (pop-move moves)
        [tail-offset offsets'] (chain-pull offsets move)
        tail-pos' (vadd tail-offset tail-pos)
        visited' (conj visited tail-pos')]
    {:tail-pos tail-pos'
     :visited visited'
     :offsets offsets'
     :moves moves'}))

(comment
  (->> sample-input
       init-rope
       (iterate run-move)
       (drop-while (comp not empty? :moves))
       first :visited count)
  (->> input
       init-rope
       (iterate run-move)
       (drop-while (comp not empty? :moves))
       first :visited count)
  
  (->> sample-input
       init-chain
       (iterate run-chain-move)
       (drop-while (comp not empty? :moves))
       first :visited count)
  (->> sample-input-2
       init-chain
       (iterate run-chain-move) 
       (drop-while (comp not empty? :moves))
       first :visited count)
  (->> input
       init-chain
       (iterate run-chain-move)
       (drop-while (comp not empty? :moves))
       first :visited count)
  )