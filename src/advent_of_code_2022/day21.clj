(ns advent-of-code-2022.day21
  (:require [advent-of-code-2022.core :as core]
            [clojure.string :as str]
            [advent-of-code-2022.left-shark :as ls]))

(def parse-line
  (comp
   :components
   (ls/parse
    (ls/! #"...." :name)
    (ls/! ": ")
    (ls/< (ls/int! :number))
    (ls/<
     (ls/! #"...." :left)
     (ls/! " ")
     (ls/! #"[-+*/]" :op {"+" + "-" - "*" * "/" /})
     (ls/! " ")
     (ls/! #"...." :right))
    ls/$)))

(def input
  (->> "day21.txt"
       core/read-input
       str/split-lines
       (map parse-line)
       (core/index-by :name)))

(def sample-input
  (->> "root: pppw + sjmn
dbpl: 5
cczh: sllz + lgvd
zczc: 2
ptdq: humn - dvpt
dvpt: 3
lfqf: 4
humn: 5
ljgn: 2
sjmn: drzm * dbpl
sllz: 4
pppw: cczh / lfqf
lgvd: ljgn * ptdq
drzm: hmdt - zczc
hmdt: 32" 
       str/split-lines
       (map parse-line)
       (core/index-by :name)))

(core/defn-split interp-fn [names | {[instr & instrs'] :instrs :keys [data] :as stacks}]
  (if-not (string? instr)
    (let [[operands data'] (split-at 2 data)]
      (-> stacks
          (assoc :data (cons (apply instr operands) data'))
          (assoc :instrs instrs')))
    (let [monkey (names instr)]
      (if (:op monkey)
        (assoc stacks :instrs (concat ((juxt :right :left :op) monkey) instrs'))
        (-> stacks
            (update :data #(cons (:number monkey) %))
            (assoc :instrs instrs'))))))

(core/defn-split interp-fn-2 [names | {[instr & instrs'] :instrs :keys [data] :as stacks}]
  (cond
    (not (string? instr))
    (let [[operands data'] (split-at 2 data)]
      (-> stacks
          (assoc :data (cons (apply instr operands) data'))
          (assoc :instrs instrs')))
    
    (= "root" instr)
    (assoc stacks :instrs (concat ((juxt :right :left) (names "root")) instrs'))
    
    :else
    (let [monkey (names instr)]
      (if (:op monkey)
        (assoc stacks :instrs (concat ((juxt :right :left :op) monkey) instrs'))
        (-> stacks
            (update :data #(cons (:number monkey) %))
            (assoc :instrs instrs'))))))

(defn humanize [names n]
  (-> names
      (assoc "humn" {:number n})
      interp-fn-2))

(comment
  (let [interp (interp-fn sample-input)]
    (->> {:instrs '("root") :data '()}
         (core/iterate-to :instrs interp)
         :data
         first))
  (let [interp (interp-fn input)]
    (->> {:instrs '("root") :data '()}
         (core/iterate-to :instrs interp)
         :data
         first))

  (for [i (range 10)
        :let [interp (humanize sample-input i)]]
    (->> {:instrs '("root") :data '()}
         (core/iterate-to :instrs interp)
         :data
         (vector i)))
  (->> {:instrs '("root") :data '()}
       (core/iterate-to :instrs (humanize sample-input 301))
       :data)

  (for [i (range 2)
        :let [interp (humanize input i)]]
    (->> {:instrs '("root") :data '()}
         (core/iterate-to :instrs interp)
         :data
         (vector i)))
  
  ;; This gives two tuples, one for humn=0 and humn=1.
  ;; Since the operations are all linear, we just need to work out the line.
  ;; Instead of trying to generalize overmuch, I'm just pulling the results out
  ;; directly from running the above.
  (let [zero (/ 176625488660456411 2025)
        one  (/ 176625488660419099 2025)
        slope (- one zero)
        target 31343426392931
        diff (- target zero)]
    (/ diff slope))
  )