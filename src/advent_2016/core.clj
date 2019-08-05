(ns advent-2016.core
  (:require [advent-2016.day01 :refer [day01 day01-part2]])
  (:require [clojure.pprint :refer [pprint]])
  (:gen-class))

(defn -main
  [& args]
  (do
    (prn (day01))
    (pprint (day01-part2))))
