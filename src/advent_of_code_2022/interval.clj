(ns advent-of-code-2022.interval
  "Open and closed integer intervals, of the structure `[start end]`")

(defn open-interval?
  "Whether `x` is a valid open interval.
   Useful for situations where inverse intervals (e.g. [5, 0) ) should be removed."
  [x]
  (and (seq? x) (= 2 (count x)) (apply < x)))

(defn closed-interval?
  "Whether `x` is a valid closed interval.
   Useful for situations where inverse intervals (e.g. [5, 0]) should be removed."
  [x]
  (and (seq? x) (= 2 (count x)) (apply <= x)))

(defn ->open-interval
  "Converts `x` to an open interval if possible, or nil."
  [x]
  (when (and (seq? x) (= 2 (count x)) (apply not= x))
    (if (apply < x) x
        [(second x) (first x)])))

(defn ->closed-interval
  "Converts `x` to an closed interval if possible, or nil."
  [x]
  (when (and (seq? x) (= 2 (count x)))
    (if (apply <= x) x
        [(second x) (first x)])))

(defn open-overlap?
  "Whether the open intervals have any overlap."
  [[start-a end-a] [start-b end-b]]
  (and (< start-a end-b) (< start-b end-a)))

(defn closed-overlap?
  "Whether the closed intervals have any overlap."
  [[start-a end-a] [start-b end-b]]
  (and (<= start-a end-b) (<= start-b end-a)))

(defn open-contiguous?
  "Whether the open intervals are contiguous - overlapping, or directly adjacent."
  [[start-a end-a :as a] [start-b end-b :as b]]
  (or (open-overlap? a b) (= start-a end-b) (= start-b end-a)))

(defn closed-contiguous?
  "Whether the closed intervals are contiguous - overlapping, or directly adjacent."
  [[start-a end-a :as a] [start-b end-b :as b]]
  (or (closed-overlap? a b) (= start-a (inc end-b)) (= start-b (inc end-a))))

(defn open-intersection
  "The intersection of open intervals, or nil."
  [[start-a end-a :as a] [start-b end-b :as b]]
  (when (open-overlap? a b)
    [(max start-a start-b) (min end-a end-b)]))

(defn closed-intersection
  "The intersection of closed intervals, or nil."
  [[start-a end-a :as a] [start-b end-b :as b]]
  (when (closed-overlap? a b)
    [(max start-a start-b) (min end-a end-b)]))

(defn cover
  "Make an interval that covers the full range of its arguments.
   Preserves open/closed."
  [interval & intervals]
  (reduce
   (fn [[start-a end-a] [start-b end-b]] [(min start-a start-b) (max end-a end-b)])
   interval
   intervals))

(defn open-split
  "Splits the interval by removing `n`, returning 0-2 ranges."
  [n [start end]]
  (cond
    (= start n (dec end)) []
    (= start n) [[(inc start) end]]
    (= (dec end) n) [[start (dec end)]]
    (or (> start n) (<= end n)) [[start end]]
    :else [[start n] [(inc n) end]]))

(defn closed-split
  "Splits the interval by removing `n`, returning 0-2 ranges."
  [n [start end]]
  (cond
    (= start n end) []
    (= start n) [[(inc start) end]]
    (= end n) [[start (dec end)]]
    (or (> start n) (< end n)) [[start end]]
    :else [[start (dec n)] [(inc n) end]]))

(defn open-carve
  "Interval subtraction, returning 0-2 ranges."
  [[start-a end-a] [start-b end-b]]
  (cond
    (<= start-b start-a end-a end-b) []
    (<= start-b end-a end-b) [[start-a start-b]]
    (<= start-b start-a end-b) [[end-b end-a]]
    :else [[start-a start-b] [end-b end-a]]))
(defn closed-carve
  "Interval subtraction, returning 0-2 ranges."
  [[start-a end-a] [start-b end-b]]
  (cond
    (<= start-b start-a end-a end-b) []
    (<= start-b end-a end-b) [[start-a (dec start-b)]]
    (<= start-b start-a end-b) [[(inc end-b) end-a]]
    :else [[start-a (dec start-b)] [(inc end-b) end-a]]))

(defn open-min
  "Limits open interval to elements starting at `n`.
   `kind` is `:keep` or `:drop`, based on whether `n` should be included in the
   resulting interval."
  [kind n [start end]]
  (case kind
    :keep
    (cond
      (>= n end) []
      (> n start) [[n end]]
      :else [[start end]])
    
    :drop
    (cond
      (>= n (dec end)) []
      (>= n start) [[(inc n) end]]
      :else [[start end]])
    
    (throw (RuntimeException. (str "Unknown `open-min` `kind` `" kind "`")))))

(defn closed-min
  "Limits closed interval to elements starting at `n`.
   `kind` is `:keep` or `:drop`, based on whether `n` should be included in the
   resulting interval."
  [kind n [start end]]
  (case kind
    :keep
    (cond
      (> n end) []
      (> n start) [[n end]]
      :else [[start end]])

    :drop
    (cond
      (>= n end) []
      (>= n start) [[(inc n) end]]
      :else [[start end]])

    (throw (RuntimeException. (str "Unknown `closed-min` `kind` `" kind "`")))))

(defn open-max
  "Limits open interval to elements ending at `n`.
   `kind` is `:keep` or `:drop`, based on whether `n` should be included in the
   resulting interval."
  [kind n [start end]]
  (case kind
    :keep
    (cond
      (< n start) []
      (< n end) [[start (inc n)]]
      :else [[start end]])

    :drop
    (cond
      (<= n start) []
      (< n end) [[start n]]
      :else [[start end]])

    (throw (RuntimeException. (str "Unknown `open-max` `kind` `" kind "`")))))

(defn closed-max
  "Limits closed interval to elements ending at `n`.
   `kind` is `:keep` or `:drop`, based on whether `n` should be included in the
   resulting interval."
  [kind n [start end]]
  (case kind
    :keep
    (cond
      (< n start) []
      (< n end) [[start n]]
      :else [[start end]])

    :drop
    (cond
      (<= n start) []
      (<= n end) [[start (dec n)]]
      :else [[start end]])

    (throw (RuntimeException. (str "Unknown `closed-max` `kind` `" kind "`")))))

(defn open-bound
  "Combine `open-min` and `open-max`."
  [min-kind min max-kind max interval]
  (->> interval
       (open-min min-kind min)
       (open-max max-kind max)))

(defn closed-bound
  "Combine `closed-min` and `closed-max`."
  [min-kind min max-kind max interval]
  (->> interval
       (closed-min min-kind min)
       (mapcat #(closed-max max-kind max %))))