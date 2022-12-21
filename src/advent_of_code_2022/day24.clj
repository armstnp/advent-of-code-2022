(ns advent-of-code-2022.day24
  (:require [advent-of-code-2022.core :as core]
            [clojure.string :as str]))

(defn parse-line [line]
  (let [[op a b] (str/split line #" ")
        base {:op (keyword op)
              :variable (keyword a)}]
    (if-not b base
            (assoc base :operand
                   (if (#{"w" "x" "y" "z"} b)
                     (keyword b)
                     (core/parse-int b))))))

(def input
  (->> "day24.txt"
       core/read-input
       str/split-lines
       (map parse-line)))

(defn operand-fn [operand]
  (if (number? operand)
    (constantly operand)
    operand))

(defn collapse [n]
  (if (and (vector? n) (= (first n) (second n)))
    (first n)
    n))

(core/defn-split flex-op [op | a b]
  (collapse
    (if (number? a)
      (if (number? b) (op a b) (mapv #(op a %) b))
      (if (number? b) (mapv #(op % b) a) (mapv op a b)))))

(defn flex-eql [a b]
  (cond
    (and (number? a) (number? b)) (if (= a b) 1 0)
    (and (number? a) (vector? b)) (recur b a)
    (and (vector? a) (number? b)) (if (and (>= b (first a)) (<= b (second a))) [0 1] 0)
    :else (if (or (> (first a) (second b)) (< (second a) (first b))) 0 [0 1])))

(core/defn-split arith-instr [{:keys [op variable operand]} | variables]
  (let [op (op {:add (flex-op +)
                :mul (flex-op *)
                :div (flex-op (comp int /))
                :mod (flex-op rem)
                :eql flex-eql})
        operand-fn (operand-fn operand)]
    (update variables variable
            #(op % (operand-fn variables)))))

(def machine-steps
  (->> input
       (core/split-over #(not= (:op %) :inp))
       rest
       (map (partial map arith-instr))))

(declare run-machine)
(defn run-machine [digits machine instructions]
  (if-not instructions
    (when (zero? (:z machine)) digits)
    (let [[step & steps] instructions]
      (->> [9 8 7 6 5 4 3 2 1]
           (map (fn [digit] (run-machine (conj digits digit) (reduce #(%2 %1) (assoc machine :w digit) step) steps)))
           (keep identity)
           first))))




(comment
  (println (run-machine [] {:w 0 :x 0 :y 0 :z 0} machine-steps))

  (->> "add z w
mod z 2
div w 2
add y w
mod y 2
div w 2
add x w
mod x 2
div w 2
mod w 2"
       str/split-lines
       (map parse-line)
       (filter #(not= (:op %) :inp))
       (map arith-instr)
       (reduce #(%2 %1) {:w 13 :x 0 :y 0 :z 0})))


;; w <-
;; if w != 13
;;   z = 26z + w + 10


;; w <-
;; if z % 26 + 11 != w
;;   z = 26z + w + 16


;; w <-
;; if z % 26 + 11 != w 
;;   z = 26z + w + 1


;; w <-
;; if z % 26 + 10 != w
;;   z = 26z + w + 13


;; w <-
;; x = z % 26 - 14
;; z /= 26
;; if x != w
;;   z = 26z + w + 7


;; w <-
;; x = z % 26 - 4
;; z /= 26
;; if x != w
;;   z = 26z + w + 11


;; w <-
;; if z % 26 + 11 != w
;;   z = 26z + w + 11


;; w <-
;; x = z % 26 - 3
;; z /= 26
;; if x != w
;;   z = 26z + w + 10


;; w <-
;; if z % 26 + 12 != w
;;   z = 26z + w + 16


;; w <-
;; x = z % 26 - 12
;; z /= 26
;; if x != w
;;   z = 26z + w + 8


;; w <-
;; if z % 26 + 13 != w
;;   z = 26z + w + 15


;; w <-
;; x = z % 26 - 12
;; z /= 26
;; if x != w
;;   z = 26z + w + 2


;; w <-
;; x = z % 26 - 15
;; z /= 26
;; if x != w
;;   z = 26z + w + 5


;; w <-
;; x = z % 26 - 12
;; z /= 26
;; assert z = 0


;; Working backwards:
;; z = 0
;; z ∊ (-26, 26)
;; N = anything -> 9

;; ((-31 - M) / 26, (21 - M) / 26) = z 