(ns advent-of-code-2022.day16
  (:require [advent-of-code-2022.core :as core]
            [clojure.string :as str]
            [advent-of-code-2022.left-shark :as ls]))

(def parse-line
  (comp
   (fn [v]
     (update v :tunnels str/split #", "))
   :components
   (ls/parse
    (ls/! "Valve ")
    (ls/! #".." :valve)
    (ls/! " has flow rate=")
    (ls/int! :flow-rate)
    (ls/! #"; tunnels? leads? to valves? ")
    (ls/! #".+" :tunnels)
    ls/$)))

(def input
  (->> "day16.txt"
       core/read-input
       str/split-lines
       (map parse-line)
       (core/index-by :valve)
       (core/map-vals #(dissoc % :valve))))

(def sample-input
  (->> "Valve AA has flow rate=0; tunnels lead to valves DD, II, BB
Valve BB has flow rate=13; tunnels lead to valves CC, AA
Valve CC has flow rate=2; tunnels lead to valves DD, BB
Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE
Valve EE has flow rate=3; tunnels lead to valves FF, DD
Valve FF has flow rate=0; tunnels lead to valves EE, GG
Valve GG has flow rate=0; tunnels lead to valves FF, HH
Valve HH has flow rate=22; tunnel leads to valve GG
Valve II has flow rate=0; tunnels lead to valves AA, JJ
Valve JJ has flow rate=21; tunnel leads to valve II"
       str/split-lines
       (map parse-line)
       (core/index-by :valve)
       (core/map-vals #(dissoc % :valve))))

(defn init-state [valves]
  {:valves valves
   :minutes 30
   :room "AA"
   :opened #{}
   :pressure 0})

(defn step-tunnel [state tunnel]
  (-> state
      (assoc :room tunnel)
      (update :minutes dec)))

(defn open-valve [{:keys [valves room minutes] :as state}]
  (let [{:keys [flow-rate]} (valves room)
        minutes' (dec minutes)]
    (-> state
        (assoc :minutes minutes')
        (update :opened conj room)
        (update :pressure + (* flow-rate minutes')))))

(defn next-states [{:keys [valves room opened] :as state}]
  (let [{:keys [flow-rate tunnels]} (valves room)
        open-valve-state (when-not (or (zero? flow-rate) (opened room))
                           [(open-valve state)])
        tunnel-states (map #(step-tunnel state %) tunnels)]
    (into tunnel-states open-valve-state)))

(defn max-capacity [{:keys [valves minutes pressure opened]}]
  (->> valves
       (filter #(not (or (opened (first %)) (zero? (:flow-rate (second %))))))
       (map (fn [[_ {:keys [flow-rate]}]] (* (dec minutes) flow-rate)))
       (reduce + pressure)))

(defn step-search [{:keys [queue max-found]}]
  (let [[state & states] queue
        nexts (filter #(> (max-capacity %) max-found) (next-states state))]
    {:queue (into states (filter #(not (zero? (:minutes %))) nexts))
     :max-found (reduce #(max %1 (:pressure %2)) max-found nexts)}))

(comment
  (->> sample-input
       init-state
       list
       (array-map :max-found 0 :queue)
       (iterate step-search)
       (drop-while #(seq (:queue %)))
       first)
  ; This will need to scale beyond just branch trimming.
  ; Compress the graph by simply measuring the distance
  ; between each significant valve.
  (->> input
       init-state
       list
       (array-map :max-found 0 :queue)
       (iterate step-search)
       (drop-while #(seq (:queue %)))
       first)
  )