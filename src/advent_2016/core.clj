(ns advent-2016.core
  (:require [clojure.string :as str])
  (:gen-class))

(defn turn-left?
  "Returns whether the instruction is to turn Left (otherwise, turn Right)"
  [inst]
  (str/starts-with? inst "L"))

(defn turn-left
  "returns the direction after turning left"
  [dir]
  (case dir
    :N :W
    :W :S
    :S :E
    :E :N
    :fail
    ))

(defn turn-right
  "returns the direction after turning left"
  [dir]
  (case dir
    :N :E
    :W :N
    :S :W
    :E :S
    :fail
    ))

(defn turn
  "Return a position with the 'turn' part of the command (L=Left, R=Right)"
  [pos inst]
  (if (turn-left? inst)
    (turn-left (:dir pos))
    (turn-right (:dir pos))))

(defn dist
  "Extract the integer distance from the instructions 'R10' => 10"
  [inst]
  (Integer. (subs inst 1)))

(defn delta
  "Returns a map with :x and :y set to what the delta (movement) in those directions"
  [pos inst]
  (let [d {:x 0 :y 0}
        dst (dist inst)]
    (case (:dir pos)
      :N (assoc d :y dst)     ; North moves up, so along the positive y axis
      :W (assoc d :x (- dst)) ; West moves left, so along the negative x axis, and so on.
      :S (assoc d :y (- dst))
      :E (assoc d :x dst)
      )))

(defn apply-delta
  "Given current pos, move position in the direction it's facing according to inst"
  [pos inst]
  (let [delt (delta pos inst)]
    (-> pos
        (update :x + (:x delt))
        (update :y + (:y delt)))))

(defn apply-turn
  "Return a position with the 'turn' (R or L) part of the instruction applied"
  [pos inst]
  (assoc pos :dir (turn pos inst)))

(defn move
  "Given a pos (a map with keys: dir x y) and move instruction ('R10'), returns a new pos"
  [pos inst]
  (-> pos (apply-turn inst) (apply-delta inst)))
; below vvv is equivalent, not sure which is better...
; (apply-delta (apply-turn pos inst) inst))

(defn get-input
  "Turns the input file into a sequence of instructions"
  []
  (->
    "resources/input01.txt"
    slurp                   ; read from file
    str/trim                ; get rid of trailing newline
    (str/split #", ")))     ; values are separated by a comma and a space

(defn abs
  "Having trouble with Math/abs, so it's just easier to code my own"
  [x]
  (if (>= x 0) x (- x)))

(defn absolute-distance
  [pos]
  (+ (abs (:x pos)) (abs (:y pos))))

(defn day01
  "Solves Day 01 of Advent of Code 2016: https://adventofcode.com/2016/day/1"
  []
  (absolute-distance
    (reduce move {:dir :N :x 0 :y 0} (get-input))))


(defn -main
  [& args]
  (prn (day01)))
