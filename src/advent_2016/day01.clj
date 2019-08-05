; Solves https://adventofcode.com/2016/day/1
; TIL: reduce has a couple of modes. Still getting familiar with it.
;
; Problems:
; - had to write my own abs fn?
; - learning pains: keeping fns pure is a new mode of thinking
;   - still getting familiar with reduce, iterate, etc. over the for-loop of C/Java/Python/etc
(ns advent-2016.day01
  (:require [clojure.string :as str])
  (:require [advent-2016.util :as util]))

(def origin {:dir :N :x 0 :y 0})

(defn turn-left?
  "Returns whether the instruction is to turn Left (otherwise, turn Right)"
  [inst]
  (str/starts-with? inst "L"))

(def dirs
  "map of directions to their Left (first) and Right (second) directions"
  {:N [:W :E] :E [:N :S] :S [:E :W] :W [:S :N]})

(defn turn-left
  "returns the direction after turning left"
  [dir]
  (first (dir dirs)))

(defn turn-right
  "returns the direction after turning left"
  [dir]
  (second (dir dirs)))

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
      :default)))

(defn apply-delta
  "Given current pos, move position in the direction pos is facing"
  [pos inst]
  (let [delt (delta pos inst)]
    (-> pos
        (update :x + (:x delt))
        (update :y + (:y delt)))))

(defn apply-turn
  "Return a position with (just) the 'turn' (R or L) part of the instruction applied"
  [pos inst]
  (assoc pos :dir (turn pos inst)))

(defn move
  "Given a pos (a map with keys: dir x y) and move instruction ('R10'), returns a new pos"
  [pos inst]
  (apply-delta (apply-turn pos inst) inst))

(defn absolute-distance
  [pos]
  (+ (util/abs (:x pos)) (util/abs (:y pos))))

(defn day01
  "Solves Day 01 of Advent of Code 2016: https://adventofcode.com/2016/day/1
  Starts at the origin and moves according to the instruction in the input file.
  Returns the absolute-distance of the final position"
  []
  (absolute-distance
   (reduce move origin (util/get-input "01"))))

(defn step-foward
  "Just moves position one step forward in the direction pos is facing"
  [pos]
  (apply-delta pos "x1"))

(defn pos-to-xy
  "Get just the x and y values out of pos. Use for the 'been' set, since only x and y matter for 
  when visiting a location twice (direction facing doesn't matter)"
  [pos]
  {:x (:x pos) :y (:y pos)})

(defn forward-with-tracing
  "Each call to this moves the pos in the direction they're facing
  Every step adds positions to the 'been' set.
  Visited represents the first location visited twice (nil if none yet)"
  [[pos been visited]]
  ; the 'been' set only consists of the x & y coord pairs, so defining xy
  (let [xy (pos-to-xy pos)]
    (if (nil? visited)
      ; continue walking until we've visited someplace twice
      [(step-foward pos) (conj been xy) (if (contains? been xy) pos nil)]
      ; once visited is non-nil, just simply no-op
      [pos been visited])))

(defn travel-with-tracing
  "pos is position, a map of {:dir :x :y}, where dir is the direction facing, x and y coordinates
  been is a set of all previous locations (just the x and y)
  visited is the FIRST location visited twice (nil if none yet)
  inst is the instruction to execute, like 'R10' which means 'turn right, then go 10"
  ; if no instruction is given, just no-op (used to work with reduce. Not sure about this though)
  ([[pos been visited]] [pos been visited])
  ; if an instruction has been given, do it!
  ([[pos been visited] inst]
   ; inc steps because first 'iterate' does nothing
   (let [steps (inc (dist inst))]
     ; the last-take-iterate feels like a bad pattern. Not sure about this.
     (last
      (take steps
            (iterate forward-with-tracing
                     [(apply-turn pos inst) been visited]))))))

(defn day01-part2
  "Returns the absolute distance of the first location visited twice after following the
  directions"
  []
  ; absolute distance of the first node visited twice is what we want
  (absolute-distance
    ; last of [pos been visited] is visited, the first position visited twice
   (last
    ; initial value is at the origin, facing north, with an empty set for been, and visited is nil
    (reduce travel-with-tracing [origin (set nil) nil] (util/get-input "01")))))
