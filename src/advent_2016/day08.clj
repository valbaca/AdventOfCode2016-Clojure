(ns advent-2016.day08)
(comment 
"Solving https://adventofcode.com/2016/day/8
 TIL:
 (repeat n x) == (take n (repeat x))
 run! to loop over seq for side-effects (like printing)
 Useful for util functions (like pscreen) to output their input for chaining
 Function reuse! By using tranpose twice, got to re-use rotate row as rotate column.

 Turning the input into a sequence of fns that could be composed into a single fn! 🤯
 No loop needed, just apply.

 As always, careful with seq vs vectors
 ")

(defn make-screen
  "Give wide (columns, x) and tall (rows, y), creates a vector of vectors of booleans (starts as false)."
  [wide tall]
  (->> false
       (repeat wide)
       vec
       (repeat tall)
       vec))

(defn- prow "prints a row, # for true and . for false" [row]
  (println (clojure.string/join (map #(if % "#" ".") row))))

(defn pscreen "prints a screen" [screen]
  (run! prow screen)
  (println)
  screen)

(defn turn-on-row
  "sets elements at indexes less than x to true"
  [row x]
  (reduce-kv
   (fn [c k v]
     (assoc c k
            (if (< k x) true v)))
   (empty row)
   row))

(defn turn-on
  "sets all positions in the upper x-y corner to true
   (sets positions where x < `x` and y < `y` to true)"
  [screen x y]
  ; this really doesn't feel right?
  (reduce-kv
   (fn [c rowi row]
     (assoc c rowi
            (if (< rowi y)
              (turn-on-row row x)
              row))) ; else, skip the row
   (vec (empty screen))
   screen))

(defn rotate-left
  "can rotate-right by giving negative n"
  [v n]
  (let [c (count v)]
    (vec (take c (drop (mod n c) (cycle v))))))

(defn rot-row
  "rotates the values in row y by n places"
  [screen y n]
  (assoc screen
         y
         (rotate-left (screen y) (- n))))

(defn transpose
  "interchanging each row and the corresponding column"
  [v]
  (apply mapv vector v))

(defn rot-col
  "rotates the values in the column x by n places"
  [screen x n]
  ; do this via transposing twice. neat.
  (transpose (rot-row (transpose screen) x n)))

(defn count-lit
  "returns the count of true elements on the screen"
  [screen]
  (->>
   screen
   flatten
   (filter identity)
   count))

(defn parse-rect
  "Parses: rect 3x2"
  [s]
  (let [splits (clojure.string/split s #"[ x]")
        x (Integer/parseInt (splits 1))
        y (Integer/parseInt (splits 2))]
    #(turn-on % x y)))

(defn parse-rotate-row
  "Parses: rotate row y=0 by 4"
  [s]
  (let [splits (clojure.string/split s #"[ =]")
        y (Integer/parseInt (splits 3))
        n (Integer/parseInt (splits 5))]
    #(rot-row % y n)))

(defn parse-rotate-col
  "Parses: rotate column x=1 by 1"
  [s]
  (let [splits (clojure.string/split s #"[ =]")
        x (Integer/parseInt (splits 3))
        n (Integer/parseInt (splits 5))]
    #(rot-col % x n)))

(defn parse-command
  "Given a line (command), returns a function like: (fn [screen])"
  [s]
  (cond (.startsWith s "rect") (parse-rect s)
        (.startsWith s "rotate row") (parse-rotate-row s)
        (.startsWith s "rotate col") (parse-rotate-col s)
        :else (throw (Exception. (str "Invalid input:" s)))))

(defn file->fn
  "Turns a file of commands into a single fn that can be applied to the starting screen"
  [filename]
  (advent-2016.util/parse-commands filename parse-command))

(count-lit
 (pscreen ((file->fn "resources/test-input08.txt")
           (make-screen 7 3))))

;; showing a few different ways to format the solution

;; straightforward, simplest
(count-lit
 (pscreen
  ((file->fn "resources/input08.txt") (make-screen 50 6))))

;; using macro
(-> (make-screen 50 6)
    ((file->fn "resources/input08.txt"))
    pscreen
    count-lit)

;; let-binding for readability?
(let [f (file->fn "resources/input08.txt")]
  (->
   (make-screen 50 6)
   f
   pscreen
   count-lit))