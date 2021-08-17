(ns advent-2016.util
  (:require [clojure.string :as str]))

(defn abs
  "Having trouble with Math/abs, so it's just easier to code my own"
  [x]
  (if (>= x 0) x (- x)))

(defn slurp-input
  "Reads file (based on standard name) into memory. NOT split"
  [day]
  (->
    (str "resources/input" day ".txt")
    slurp
    str/trim))

(defn get-input
  "Reads input and splits on comma-space"
  [day]
    (str/split (slurp-input day) #", "))

(defn rcomp
  "reverse composition, when you want to execute left-to-right"
  [& args]
  (apply comp (reverse args)))

(defn parse-commands
  "given a filename and a line-parser (which should turn a line into a fn) returns a "
  [filename line-parser]
  (->>
   filename
   slurp
   clojure.string/split-lines
   (map line-parser)
   (apply rcomp)
   ))
