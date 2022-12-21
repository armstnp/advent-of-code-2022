(ns advent-of-code-2022.day19
  (:require [advent-of-code-2022.core :as core]
            [clojure.string :as str]
            [advent-of-code-2022.left-shark :as ls]))

; All sets - resources, robots, costs - are four-tuples of [ore clay obs geode]

(def parse-line
  (comp
   (fn [{:keys [id ore-ore clay-ore obsidian-ore obsidian-clay geode-ore geode-obsidian]}]
     {:id id
      :costs [[ore-ore 0 0 0]
              [clay-ore 0 0 0]
              [obsidian-ore obsidian-clay 0 0]
              [geode-ore 0 geode-obsidian 0]]})
   :components
   (ls/parse
    (ls/! "Blueprint ") (ls/int! :id)
    (ls/! ": Each ore robot costs ") (ls/int! :ore-ore)
    (ls/! " ore. Each clay robot costs ") (ls/int! :clay-ore)
    (ls/! " ore. Each obsidian robot costs ") (ls/int! :obsidian-ore)
    (ls/! " ore and ") (ls/int! :obsidian-clay)
    (ls/! " clay. Each geode robot costs ") (ls/int! :geode-ore)
    (ls/! " ore and ") (ls/int! :geode-obsidian)
    (ls/! " obsidian.") ls/$)))

(def input
  (->> "day19.txt"
       core/read-input
       str/split-lines
       (map parse-line)))

(def sample-input
  [{:id 1
    :costs [[4 0 0 0]
            [2 0 0 0]
            [3 14 0 0]
            [2 0 7 0]]}
   {:id 2
    :costs [[2 0 0 0]
            [3 0 0 0]
            [3 8 0 0]
            [3 0 12 0]]}])

(defn can-build? [resources cost]
  (every? identity (map >= resources cost)))

(defn capped? [robots costs]
  (mapv #(every? (fn [cost] (>= %1 cost)) %2)
        robots
        (core/transpose costs)))

(def type->index
  {:ore 0
   :clay 1
   :obsidian 2
   :geode 3})
(core/defn-split build? [all-costs | {:keys [resources robots]} typ]
  (let [index (type->index typ)]
   (and (not (nth (capped? robots all-costs) index))
        (can-build? resources (nth all-costs index)))))

(defn tuple-op [op]
  (fn [& xs] (apply mapv op xs)))
(def tuple-+ (tuple-op +))
(def tuple-- (tuple-op -))

(defn build [state cost build-tuple]
  (-> state
      (update :resources tuple-- cost)
      (update :robots tuple-+ build-tuple)))

(defn stepper [[ore-cost clay-cost obsidian-cost geode-cost :as all-costs]]
  (let [build? (build? all-costs)]
    (fn step-time [{:keys [resources robots] :as state}]
      (let [build-nothing (-> state
                              (update :time dec)
                              (update :resources tuple-+ robots))]
        (cond
          (can-build? resources geode-cost)
          [(build build-nothing geode-cost [0 0 0 1])]

          (build? state :obsidian)
          [(build build-nothing obsidian-cost [0 0 1 0])]
          
          :else
          (let [ore-state (when (build? state :ore)
                            [(build build-nothing ore-cost [1 0 0 0])])
                clay-state (when (build? state :clay)
                             [(build build-nothing clay-cost [0 1 0 0])])]
            (concat clay-state ore-state [build-nothing])))))))

(defn potential-geodes [{[_ _ _ geodes] :resources
                         [_ _ _ robots] :robots
                         :keys [time]}]
  (+ geodes (* robots time) (/ (* time (dec time)) 2)))

(def init-state-a {:resources [0 0 0 0] :robots [1 0 0 0] :time 24})
(def init-state-b {:resources [0 0 0 0] :robots [1 0 0 0] :time 32})

(defn find-max-geodes [init-state blueprint]
  (let [stepper (stepper (:costs blueprint))]
    (loop [max-geodes 0
           [{:keys [time resources] :as next} & queue'] [init-state]]
      (cond
        (not next) max-geodes

        (zero? time)
        (recur (max max-geodes (nth resources 3)) queue')

        (< (potential-geodes next) max-geodes)
        (recur max-geodes queue')

        :else
        (recur max-geodes (concat (stepper next) queue'))))))

(comment
  (reduce + (map #(* (:id %) (find-max-geodes init-state-a %)) sample-input))
  (reduce + (pmap #(* (:id %) (find-max-geodes init-state-a %)) input))
  
  
  (pmap #(find-max-geodes init-state-b %) sample-input)
  (pmap #(find-max-geodes init-state-b %) (take 3 input))
  )