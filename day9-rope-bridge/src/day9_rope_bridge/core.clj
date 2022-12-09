(ns day9-rope-bridge.core
  (:require [clojure.string :as string]))

(def directions
  {"R" :right
   "U" :up
   "L" :left
   "D" :down})

(defn parse-move [move-line]
  (let [[_ direction amount] (re-matches #"([LRUD]) (\d+)$" move-line)]
    (repeat (Integer/parseInt amount) (directions direction))))

(defn calculate-direction [[x1 y1] [x2 y2]]
  [(- x2 x1) (- y2 y1)])

(defn close? [[ox oy]]
  (and
    (<= (Math/abs ^int ox) 1)
    (<= (Math/abs ^int oy) 1)))

(defn sign-of [x]
  (cond
    (zero? x) 0
    (neg? x) -1
    :else 1))

(defn move-closer [[ox oy] [tx ty]]
  (let [ox (sign-of ox)
        oy (sign-of oy)]
    [(+ tx ox) (+ ty oy)]))

(defn move-pair [[dx dy] [hx hy] [tx ty :as tail]]
  (let [new-head [(+ hx dx) (+ hy dy)]
        [nhx nhy] new-head
        offset [(- nhx tx) (- nhy ty)]
        new-tail (if (close? offset)
                   tail
                   (move-closer offset tail))]
    [new-head new-tail]))

(defn move-rope [direction rope]
  (loop [rope (conj rope [0 0])
         direction direction
         new-rope []]
    (if (= 1 (count rope))
      new-rope
      (let [[head tail] (take 2 rope)
            [new-head new-tail] (move-pair direction head tail)
            new-direction (calculate-direction tail new-tail)]
        (recur (rest rope) new-direction (conj new-rope new-head))))))

(def direction-deltas
  {
   :right [1 0]
   :left [-1 0]
   :up [0 1]
   :down [0 -1]})

(defn track-tail-of-rope [file-name n]
  (let [lines (string/split-lines (slurp file-name))
        moves (map direction-deltas (flatten (map parse-move lines)))]
    (loop [moves moves
           rope (repeat n [0 0])
           tails #{[0 0]}]
      (if (empty? moves)
        tails
        (let [new-rope (move-rope (first moves) rope)]
          (recur (rest moves) new-rope (conj tails (last new-rope))))))))


