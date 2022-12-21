(ns advent-of-code-2022.day22
  (:require [advent-of-code-2022.core :as core]
            [clojure.string :as str]
            [advent-of-code-2022.left-shark :as ls]))

(def parser
  (comp :components
        (ls/parse
         (ls/! #"on|off" :op keyword)
         (ls/! " x=")
         (ls/int! :xmin)
         (ls/! "..")
         (ls/int! :xmax)
         (ls/! ",y=")
         (ls/int! :ymin)
         (ls/! "..")
         (ls/int! :ymax)
         (ls/! ",z=")
         (ls/int! :zmin)
         (ls/! "..")
         (ls/int! :zmax))))

(defn parse-line [line]
  (let [{:keys [op xmin xmax ymin ymax zmin zmax]} (parser line)]
    [op [xmin xmax] [ymin ymax] [zmin zmax]]))

(def input
  (->> "day22.txt"
       core/read-input
       str/split-lines
       (map parse-line)))

;; Part 1 Solution

(defn intersection [[[xmina xmaxa] [ymina ymaxa] [zmina zmaxa]]
                    [[xminb xmaxb] [yminb ymaxb] [zminb zmaxb]]]
  (let [xmini (max xmina xminb)
        xmaxi (min xmaxa xmaxb)
        ymini (max ymina yminb)
        ymaxi (min ymaxa ymaxb)
        zmini (max zmina zminb)
        zmaxi (min zmaxa zmaxb)]
    (when (and (<= xmini xmaxi) (<= ymini ymaxi) (<= zmini zmaxi))
      [[xmini xmaxi] [ymini ymaxi] [zmini zmaxi]])))

(defn intersect [[sign & existing] new]
  (when-let [icube (intersection existing new)]
    (cons (- sign) icube)))

(defn add-cube [cubes [op & new]]
  (if (= op :on)
    (let [icubes (keep #(intersect % new) cubes)]
      (concat cubes icubes [(cons 1 new)]))
    (let [icubes (keep #(intersect % new) cubes)]
      (concat cubes icubes))))

(defn volume [[sign [xmin xmax] [ymin ymax] [zmin zmax]]]
  (* sign (inc (- xmax xmin)) (inc (- ymax ymin)) (inc (- zmax zmin))))

(->> input
     (filter #(intersection (rest %) [[-50 50] [-50 50] [-50 50]]))
     (reduce add-cube [])
     (map volume)
     (reduce +))

(comment
  (->> "on x=-20..26,y=-36..17,z=-47..7
on x=-20..33,y=-21..23,z=-26..28
on x=-22..28,y=-29..23,z=-38..16
on x=-46..7,y=-6..46,z=-50..-1
on x=-49..1,y=-3..46,z=-24..28
on x=2..47,y=-22..22,z=-23..27
on x=-27..23,y=-28..26,z=-21..29
on x=-39..5,y=-6..47,z=-3..44
on x=-30..21,y=-8..43,z=-13..34
on x=-22..26,y=-27..20,z=-29..19
off x=-48..-32,y=26..41,z=-47..-37
on x=-12..35,y=6..50,z=-50..-2
off x=-48..-32,y=-32..-16,z=-15..-5
on x=-18..26,y=-33..15,z=-7..46
off x=-40..-22,y=-38..-28,z=23..41
on x=-16..35,y=-41..10,z=-47..6
off x=-32..-23,y=11..30,z=-14..3
on x=-49..-5,y=-3..45,z=-29..18
off x=18..30,y=-20..-8,z=-3..13
on x=-41..9,y=-7..43,z=-33..15
on x=-54112..-39298,y=-85059..-49293,z=-27449..7877
on x=967..23432,y=45373..81175,z=27513..53682"
       str/split-lines
       (map parse-line)
       (filter #(intersection (rest %) [[-50 50] [-50 50] [-50 50]]))
       (reduce add-cube [])
       (map volume)
       (reduce +))
  )
;; Part 2 Solution

(->> input
     (reduce add-cube [])
     (map volume)
     (reduce +))
