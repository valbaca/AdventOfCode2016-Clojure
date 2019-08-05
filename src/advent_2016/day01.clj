(ns advent-2016.day01
  (:require [clojure.string :as str])
  (:require [advent-2016.util :as util]))

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
      :default
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


(defn absolute-distance
  [pos]
  (+ (util/abs (:x pos)) (util/abs (:y pos))))

(defn day01
  "Solves Day 01 of Advent of Code 2016: https://adventofcode.com/2016/day/1"
  []
  (absolute-distance
    (reduce move {:dir :N :x 0 :y 0} (util/get-input "01"))))


; start with pos=(N, 0, 0), been-to=set #{}, visited=nil, instruction=(input)
;   newPos, dist from instruction
;   turn to newPos
;   while dist > 0 and visited != nil:
;     if set contains pos and visited is not nil: 
;       visited := pos
;     else add pos to been-to, move pos, decrement dist

(defn step-foward
  "Just moves position one forward from where they're facing"
  [pos]
  (apply-delta pos "x1"))


(defn pos-to-xy
  [pos]
  {:x (:x pos) :y (:y pos)})


(defn forward-with-tracing
  "Each call to this moves the pos in the direction they're facing, while adding positions to the been set. Visited represents the first location visited twice (nil if none yet)"
  [[pos been visited]]
  (let [xy (pos-to-xy pos)] ; the been set only consists of the x & y coord pairs
    (if (nil? visited)
      ; move forward, add 
      [(step-foward pos) (conj been xy) (if (contains? been xy) pos nil)]
      ; else just 
      [pos been visited])))


(defn travel-with-tracing
  "pos is a map of {:dir :x :y}, 
  been is a set of previous locations
  visited is the location first visited twice (nil of not yet)
  inst is the instruction to execute, like 'R10' for 'turn right, then go 10"
  [pos been visited inst]
  (let [d (dist inst)]
    (take (inc d)
      (iterate forward-with-tracing
        [(apply-turn pos inst) been visited]))))

(defn day01-part2
  []
  (travel-with-tracing {:dir :N :x 0 :y 0} (set nil) nil "R10"))
