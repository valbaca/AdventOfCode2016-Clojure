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

