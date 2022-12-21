(ns advent-of-code-2022.day17 
  (:require [advent-of-code-2022.core :as core]))

(def input (core/read-input "day17.txt"))
(def sample-input ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>")

(def shapes
  [[[0 0 1 1 1 1 0]]
   
   [[0 0 0 1 0 0 0]
    [0 0 1 1 1 0 0]
    [0 0 0 1 0 0 0]]
   
   [[0 0 0 0 1 0 0]
    [0 0 0 0 1 0 0]
    [0 0 1 1 1 0 0]]
   
   [[0 0 1 0 0 0 0]
    [0 0 1 0 0 0 0]
    [0 0 1 0 0 0 0]
    [0 0 1 0 0 0 0]]
   
   [[0 0 1 1 0 0 0]
    [0 0 1 1 0 0 0]]])

(defn strip-blank [field]
  (drop-while #(= [0 0 0 0 0 0 0] %) field))

(defn start-shape [field shape]
  (let [empty-line [0 0 0 0 0 0 0]
        stripped (strip-blank field)
        gap (repeat (+ 3 (count shape)) empty-line)]
    (into stripped gap)))

(defn merge-shape [field shape]
  (map (partial map +) field shape))

(defn blocked? [field shape]
  (or (< (count field) (count shape))
       (some #{2} (apply concat (merge-shape field shape)))))

(defn on-left-wall? [shape]
  (->> shape (map first) (reduce +) zero? not))
(defn on-right-wall? [shape]
  (->> shape (map last) (reduce +) zero? not))

(defn push-left [shape]
  (if (on-left-wall? shape)
    shape
    (map (fn [row] (concat (rest row) [0])) shape)))
(defn push-right [shape]
  (if (on-right-wall? shape)
    shape
    (map (fn [row] (->> row (cons 0) (drop-last 1) seq)) shape)))

(def wind-map
  {\< push-left
   \> push-right})

(defn step-wind [{:keys [wind field shape] :as state}]
  (let [shape' ((wind-map (first wind)) shape)
        shape' (if (blocked? field shape') shape shape')]
    (-> state
        (update :wind rest)
        (assoc :shape shape'))))

(defn compact-field [shape field]
  (if-let [active-index
           (some->>
            field
            (take (inc (count shape)))
            (partition 2 1)
            (map-indexed (fn [i [t b]] [i (every? #{1} (map max t b))]))
            (filter second)
            first first inc)]
    (update (split-at active-index field) 1 count)
    [field 0]))

(defn step-fall [{:keys [field shape field-pop] :as state}]
  (let [field' (rest field)]
    (if (blocked? field' shape)
      (let [[top-field bottom-field] (split-at (count shape) field)
            merged-field (->> shape
                              (merge-shape top-field)
                              reverse
                              (into bottom-field))
            [compacted-field compacted-height] (compact-field shape merged-field)]
        (-> state
            (assoc :rested true)
            (assoc :height compacted-height)
            (assoc :field (into compacted-field field-pop))))
      (-> state
          (update :field-pop #(cons (first field) %))
          (assoc :field field')))))

(defn drop-shape [{:keys [field shapes wind] :as state}]
  (let [shape (first shapes) 
        {:keys [field wind height]}
        (core/iterate-to
         (complement :rested)
         (comp step-fall step-wind)
         {:field (start-shape field shape)
          :shape shape
          :field-pop '()
          :wind wind})]
    (-> state
        (update :shapes rest)
        (assoc :field (strip-blank field))
        (assoc :wind wind)
        (update :height + height))))

(defn init-state [wind]
  {:field '() :shapes (cycle shapes) :wind (cycle wind) :height 0})

(comment
  (->> sample-input
       init-state
       (iterate drop-shape)
       (drop 2022)
       first
       ((juxt (comp count :field) :height))
       (apply +))
  (->> input
       init-state
       (iterate drop-shape)
       (drop 2022)
       first
       ((juxt (comp count :field) :height))
       (apply +))

  ; Yeah, that ain't gonna work. It's another cycle detection puzzle.
  ; Use better board compression (detect unreachables by scanning from the bottom)
  ; and then do a hard equality comparison with prior known states.
  ; This also means shapes and wind need to be cycled by hand
  ; instead of using `cycle`.
  (->> sample-input
       init-state
       (iterate drop-shape)
       (drop 1000000000000)
       first
       ((juxt (comp count :field) :height))
       (apply +))
  )