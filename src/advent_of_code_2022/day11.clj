(ns advent-of-code-2022.day11
  (:require [advent-of-code-2022.core :as core]
            [clojure.string :as str]
            [advent-of-code-2022.left-shark :as ls]))

(def input
  (->> "day11.txt"
       core/read-input
       str/split-lines))

(def sample-input
  (->> "day11-sample.txt"
       core/read-input
       str/split-lines))

(def parse-id (core/only-int-fn :id))
(defn parse-items [line]
  {:items (mapv core/parse-int
                (str/split (subs line 18) #", "))})
(def parse-operation
  (comp :components
        (ls/parse
         (ls/! "  Operation: new = old ")
         (ls/! #"[+*]" :op)
         (ls/! " ")
         (ls/int? :arg)
         (ls/? "old" :arg) ; Good enough 'alternative' for this case
         ls/$)))
(def parse-divisor (core/only-int-fn :divisor))
(def parse-true-id (core/only-int-fn :true-id))
(def parse-false-id (core/only-int-fn :false-id))
(defn parse-monkey [lines]
  (apply
   merge
   (map
    #(%1 %2)
    [parse-id parse-items parse-operation parse-divisor parse-true-id parse-false-id]
    lines)))

(def op-map {"+" + "*" *})
(defn op-fn [{:keys [op arg]}]
  (let [op (op-map op)]
    (if (= arg "old")
      #(op % %)
      #(op % arg))))
(defn translate-op [monkey]
  (-> monkey
      (assoc :op (op-fn monkey))
      (dissoc :arg)))

(def ^:dynamic *worry-fn* #(quot % 3))
(defn translate-inspection [{:keys [op divisor lcm-divisors true-id false-id] :as monkey}]
  (letfn [(inspect [item]
            (let [item' (-> item op *worry-fn* (mod lcm-divisors))
                  divides? (zero? (mod item' divisor))]
              [item'
               (if divides? true-id false-id)]))]
    (-> monkey
        (assoc :inspect inspect)
        (dissoc :op :divisor :lcm-divisors :true-id :false-id))))

(defn inspect-items
  "Tuple
   1. Thrown: `[{id items}]`
   2. `monkey'`"
  [{:keys [items inspect] :as monkey}]
  [(->> items
        (map inspect)
        (group-by second)
        (core/map-vals #(map first %)))
   (-> monkey
       (assoc :items [])
       (update :count + (count items)))])

(defn build-monkeys [line-groups]
  (let [raw-monkeys (map parse-monkey line-groups)
        lcm-divisors (reduce * (map :divisor raw-monkeys))
        base-monkeys (map #(assoc % :lcm-divisors lcm-divisors) raw-monkeys)]
    (->> base-monkeys 
         (map (comp #(assoc % :count 0) translate-inspection translate-op))
         (sort-by :id)
         vec)))

(defn run-inspection [monkeys n]
  (let [monkey (get monkeys n)
        [thrown monkey'] (inspect-items monkey)]
    (reduce 
     (fn [monkeys [id items]]
       (update-in monkeys [id :items] into items))
     (assoc monkeys n monkey')
     thrown)))

(defn run-all-inspections [monkeys]
  (reduce run-inspection monkeys (range (count monkeys))))

(defn monkey-business [rounds monkeys]
  (->> monkeys
       (iterate run-all-inspections)
       (drop rounds)
       first
       (map :count) sort reverse (take 2) (apply *)))

(comment
  (->> sample-input
       (core/split-over (complement str/blank?))
       build-monkeys
       (monkey-business 20))
  (->> input
       (core/split-over (complement str/blank?))
       build-monkeys
       (monkey-business 20))

  (with-bindings {#'*worry-fn* identity}
    (->> sample-input
         (core/split-over (complement str/blank?))
         build-monkeys
         (monkey-business 10000)))
  (with-bindings {#'*worry-fn* identity}
    (->> input
         (core/split-over (complement str/blank?))
         build-monkeys
         (monkey-business 10000)))
  )