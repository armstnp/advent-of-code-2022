(ns advent-of-code-2022.day15
  (:require [advent-of-code-2022.core :as core]
            [clojure.string :as str]
            [advent-of-code-2022.left-shark :as ls]
            [advent-of-code-2022.interval :as iv]))

(def parse-line
  (comp #(array-map :sensor (vec ((juxt :sx :sy) %))
                    :beacon (vec ((juxt :bx :by) %)))
        :components
        (ls/parse
         (ls/! "Sensor at x=") (ls/int! :sx)
         (ls/! ", y=") (ls/int! :sy)
         (ls/! ": closest beacon is at x=") (ls/int! :bx)
         (ls/! ", y=") (ls/int! :by)
         ls/$)))

(defn inject-distances [readings]
  (map #(assoc % :distance (core/manhattan-2 (:sensor %) (:beacon %))) readings))

(def input
  (->> "day15.txt"
       core/read-input
       str/split-lines
       (map parse-line)
       inject-distances))

(def sample-input
  (->> "Sensor at x=2, y=18: closest beacon is at x=-2, y=15
Sensor at x=9, y=16: closest beacon is at x=10, y=16
Sensor at x=13, y=2: closest beacon is at x=15, y=3
Sensor at x=12, y=14: closest beacon is at x=10, y=16
Sensor at x=10, y=20: closest beacon is at x=10, y=16
Sensor at x=14, y=17: closest beacon is at x=10, y=16
Sensor at x=8, y=7: closest beacon is at x=2, y=10
Sensor at x=2, y=0: closest beacon is at x=2, y=10
Sensor at x=0, y=11: closest beacon is at x=2, y=10
Sensor at x=20, y=14: closest beacon is at x=25, y=17
Sensor at x=17, y=20: closest beacon is at x=21, y=22
Sensor at x=16, y=7: closest beacon is at x=15, y=3
Sensor at x=14, y=3: closest beacon is at x=15, y=3
Sensor at x=20, y=1: closest beacon is at x=15, y=3"
       str/split-lines
       (map parse-line)
       inject-distances))

(defn coverage-at [{[x y] :sensor :keys [distance]} row]
  (let [width (- distance (Math/abs (- y row)))]
    (if-not (neg? width)
      [(- x width) (+ x width)]
      nil)))

(defn merge-overlapping [coverages]
  (reduce
   (fn [accum coverage]
     (apply conj
            (reduce
             (fn [[unmerged merged] item]
               (if (iv/closed-contiguous? merged item)
                 [unmerged (iv/cover merged item)]
                 [(conj unmerged item) merged]))
             [[] coverage]
             accum)))
   []
   coverages))

(defn beacons-on-row [scans row]
  (->> scans (map :beacon) (filter #(= (second %) row)) set))

(defn split-by-beacons [beacons coverages]
  (reduce
   (fn [coverages beacon]
     (mapcat
      #(iv/closed-split (first beacon) %)
      coverages))
   coverages
   beacons))

(defn coverage-at-row [row input]
  (->> input
       (keep #(coverage-at % row))
       merge-overlapping
       (split-by-beacons (beacons-on-row input row))))

(defn scan-all-rows [max-row input]
  (map
   #(sort (coverage-at-row % input))
   (range (inc max-row))))

(defn trim-to-window [max-coord coverages]
  (map
   (fn [line]
     (->> line
          (filter (partial iv/closed-overlap? [0 max-coord]))
          (mapcat (partial iv/closed-bound :keep 0 :keep max-coord))))
   coverages))

(defn seek-gaps [max-coord coverages]
  (->> coverages
       (map-indexed vector)
       (mapcat
        (fn [[row line]]
          (when (zero? (mod row 100000)) (println "Milestone: " row))
          (->> [(inc max-coord) (inc max-coord)]
               (conj (into [[-1 -1]] line))
               merge-overlapping
               (partition 2 1)
               (mapcat (fn [[[_ end] [start _]]] (range (inc end) start)))
               (map #(vector % row)))))))

(defn remove-existing-beacons [input beacons]
  (filter (->> input (map :beacon) set complement) beacons))

(defn find-missing-beacon [max-coord input]
  (->> input
       (scan-all-rows max-coord)
       (trim-to-window max-coord)
       (seek-gaps max-coord)
       (remove-existing-beacons input)
       first))

(defn tuning-frequency [[x y]]
  (+ y (* x 4000000)))

(comment
  (->> sample-input
       (coverage-at-row 10)
       (map (fn [[start end]] (inc (- end start))))
       (reduce +))
  (->> input
       (coverage-at-row 2000000)
       (map (fn [[start end]] (inc (- end start))))
       (reduce +))
  
  (->> sample-input (find-missing-beacon 20) tuning-frequency)
  (->> input (find-missing-beacon 4000000) tuning-frequency)
  )