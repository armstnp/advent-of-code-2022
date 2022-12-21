(defproject advent-of-code-2022 "0.0.1-SNAPSHOT"
  :description "Code for solving the 'Advent of Code 2022' problem set."
  :dependencies [[org.clojure/clojure "1.10.3"]
                 [is-prime "0.1.0"] ; https://github.com/fardog/is-prime
                 [org.jgrapht/jgrapht-core "1.3.0"]
                 [org.clojure/math.combinatorics "0.1.6"]
                 [quil "3.1.0"]]
  :javac-options ["-target" "1.8" "-source" "1.8" "-Xlint:-options"]
  :aot [advent-of-code-2022.core])
