(ns advent-of-code-2022.day7
  (:require [advent-of-code-2022.core :as core]
            [clojure.string :as str]
            [advent-of-code-2022.left-shark :as ls]
            [clojure.zip :as z]))

(def input
  (->> "day7.txt"
       core/read-input
       str/split-lines))

(def sample-input
  (->> "$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k"
       str/split-lines))

(defn parse-command [line]
  (case line
    "$ ls" [:ls]
    "$ cd .." [:up]
    [:down (subs line 5)]))

(def parse-file
  (comp (fn [file] [:file file])
        :components
        (ls/parse
         (ls/int! :size)
         (ls/! " ")
         (ls/! #".+" :name))))

(defn parse-listing [line]
  (if (str/starts-with? line "dir")
    [:dir (subs line 4)]
    (parse-file line)))

(defn parse-line [line]
  ((if (str/starts-with? line "$")
     parse-command
     parse-listing)
   line))

(core/defn-split run-down [[_ dir] | state]
  (update state :path conj :dirs dir))

(defn run-up [state]
  (update state :path (comp pop pop))) ; In the attic

(defn run-ls [state]
  state)

(core/defn-split run-dir [[_ dir] | {:keys [path] :as state}]
  (update-in state (conj (into [:fs] path) :dirs dir) #(or % {})))

(core/defn-split run-file [[_ {:keys [name size]}] | {:keys [path] :as state}]
  (assoc-in state (into [:fs] (conj path :files name)) size))

(defn command-fn [[tag :as command]]
  (case tag
    :down (run-down command)
    :up run-up
    :ls run-ls
    :dir (run-dir command)
    :file (run-file command)))

(defn all-sizes [path fs] ; -> [path size nested-sizes]
  (let [file-sizes (reduce + 0 (vals (:files fs)))
        dir-sizes (map (fn [[name d]] (all-sizes (conj path name) d)) (:dirs fs))
        total-size (+ file-sizes (reduce (fn [sum [_ size _]] (+ sum size)) 0 dir-sizes))
        all-sizes (reduce #(into %1 (get %2 2)) [] dir-sizes)]
    [path total-size (conj all-sizes [path total-size])]))

(comment
  (->> sample-input
       (map parse-line)
       (map command-fn)
       (reduce #(%2 %1) {:path []})
       :fs
       (all-sizes [])
       (drop 2) first
       (map second)
       (filter #(<= % 100000))
       (reduce + 0))
  
  (->> input
       (map parse-line)
       (map command-fn)
       (reduce #(%2 %1) {:path []})
       :fs
       (all-sizes [])
       (drop 2) first
       (map second)
       (filter #(<= % 100000))
       (reduce + 0))
  
  (let [[_ used sizes] (->> sample-input
                            (map parse-line)
                            (map command-fn)
                            (reduce #(%2 %1) {:path []})
                            :fs
                            (all-sizes []))
        already-free (- 70000000 used )
        to-free (- 30000000 already-free)]
    (->> sizes
         (map second)
         (filter #(>= % to-free))
         (apply min)))
  
  (let [[_ used sizes] (->> input
                            (map parse-line)
                            (map command-fn)
                            (reduce #(%2 %1) {:path []})
                            :fs
                            (all-sizes []))
        already-free (- 70000000 used)
        to-free (- 30000000 already-free)]
    (->> sizes
         (map second)
         (filter #(>= % to-free))
         (apply min)))
  )