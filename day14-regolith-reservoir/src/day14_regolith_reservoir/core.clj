(ns day14-regolith-reservoir.core
  (:require [clojure.string :as string]
            [clojure.set :as set]))

(defn sand-falls-one-step [cave sand-position]
  (let [[sx sy] sand-position
        straight-down [sx (inc sy)]]
    (if (contains? cave straight-down)
      (let [diagonally-left [(dec sx) (inc sy)]]
        (if (contains? cave diagonally-left)
          (let [diagonally-right [(inc sx) (inc sy)]]
            (if (contains? cave diagonally-right)
              nil
              diagonally-right))
          diagonally-left))
      straight-down)))

(defn drop-sand
  ([cave sand-position]
   (drop-sand cave sand-position (apply max (map second cave))))
  ([cave sand-position limit]
   (if (> (second sand-position) limit)
     :void
     (let [new-pos (sand-falls-one-step cave sand-position)]
       (if (nil? new-pos)
         sand-position
         (recur cave new-pos limit))))))

(defn add-sand [cave sand-position]
  (let [new-pos (drop-sand cave sand-position)]
    (if (= :void new-pos)
      [:void cave]
      [new-pos (conj cave new-pos)])))

(defn drop-til-target
  ([cave pos target]
   (drop-til-target cave pos target 0))

  ([cave pos target n]
   (let [[new-sand cave-with-drop] (add-sand cave pos)]
     (prn new-sand)
     (if (= target new-sand)
       [n cave-with-drop]
       (recur cave-with-drop pos target (inc n))))))

(defn parse-coord [s]
  (let [coords (string/split s #",")]
    (map #(Integer/parseInt %) coords)))

(defn parse-wall-vertices [line]
  (let [coords (string/split line #" -> ")]
    (map parse-coord coords)))

(defn build-segment [[x1 y1] [x2 y2]]
  (cond
    (= x1 x2)
    (let [maxy (max y1 y2)
          miny (min y1 y2)
          ys (range miny (inc maxy))]
      (set (map #(vector x1 %) ys)))

    (= y1 y2)
    (let [maxx (max x1 x2)
          minx (min x1 x2)
          xs (range minx (inc maxx))]
      (set (map #(vector % y1) xs)))

    :else
    "TILT"))

(defn build-wall
  ([vertices]
   (build-wall vertices #{}))

  ([vertices wall]
   (if (= 1 (count vertices))
     wall
     (let [segment-vertices (take 2 vertices)
           segment (apply build-segment segment-vertices)
           new-wall (set/union wall segment)]
       (recur (rest vertices) new-wall)))))

(defn count-remaining-sand [file-name]
  (let [lines (string/split-lines (slurp file-name))
        segments (map parse-wall-vertices lines)
        walls (map build-wall segments)
        cave (apply set/union walls)
        [n cave] (drop-til-target cave [500 0] :void)]
    n))

(defn count-til-blocked [file-name]
  (let [lines (string/split-lines (slurp file-name))
        segments (map parse-wall-vertices lines)
        walls (map build-wall segments)
        cave (apply set/union walls)
        limit (apply max (map second cave))
        floor (set (map #(vector % (+ 2 limit)) (range -1000 1000)))
        cave (set/union cave floor)
        [n cave] (drop-til-target cave [500 0] [500 0])]
    (inc n)))
