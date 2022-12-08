(ns day8-treetop-tree-house.core
  (:require [clojure.string :as string]
            [clojure.set :as set]))

(defn to-row [line]
  (map #(Integer/parseInt (str %)) line))

(defn parse-forest [forest-input]
  (let [tree-rows (string/split-lines forest-input)]
    (map to-row tree-rows)))

(defn count-trees [forest]
  (* (count forest) (count (first forest))))

(defn bounds-of [forest]
  [(count (first forest)) (count forest)])

(defn rotate-forest [forest]
  (apply mapv vector forest))

(defn hidden-from-left [row]
  (loop [row row
         hidden #{}
         current-max -1
         pos 0]
    (if (empty? row)
      hidden
      (let [tree (first row)]
        (if (> tree current-max)
          (recur (rest row) hidden tree (inc pos))
          (recur (rest row) (conj hidden pos) current-max (inc pos)))))))

(defn hidden-from-right [row]
  (let [n (count row)
        reversed-positions (hidden-from-left (reverse row))]
    (set (map #(- (dec n) %) reversed-positions))))

(defn hidden-in-row [row]
  (set/intersection (hidden-from-left row) (hidden-from-right row)))

(defn find-hidden-trees-by-row [forest]
  (loop [forest forest
         hidden #{}
         row-coord 0]
    (if (empty? forest)
      hidden
      (let [row (first forest)
            hidden-this-row (hidden-in-row row)
            hidden-coords (set (map #(vector % row-coord) hidden-this-row))]
        (recur (rest forest) (set/union hidden hidden-coords) (inc row-coord))))))

(defn find-hidden-trees-by-column [forest]
  (let [rotated-forest (rotate-forest forest)
        rotated-hidden (find-hidden-trees-by-row rotated-forest)]
    (set (map reverse rotated-hidden))))

(defn find-hidden-trees [forest]
  (let [hidden-by-row (find-hidden-trees-by-row forest)
        hidden-by-column (find-hidden-trees-by-column forest)]
    (set/intersection hidden-by-row hidden-by-column)))

(defn count-visible-trees [file-name]
  (let [input (slurp file-name)
        forest (parse-forest input)
        hidden-trees (find-hidden-trees forest)
        total-trees (count-trees forest)]
    (- total-trees (count hidden-trees))))

(defn create-rays [bounds start]
  (let [[w h] bounds
        [x y] start]
    (set
      [(for [rx (range x w)] [rx y])
       (for [rx (range x -1 -1)] [rx y])
       (for [ry (range y h)] [x ry])
       (for [ry (range y -1 -1)] [x ry])])))

(defn get-tree [forest coord]
  (let [[x y] coord
        row (nth forest y)]
    (nth row x)))

(defn get-scenic-score-of-ray [forest ray]
  (let [trees-in-ray (map #(get-tree forest %) ray)
        target-tree (first trees-in-ray)]
    (loop [distant-trees (rest trees-in-ray)
           distance 0]
      (if (empty? distant-trees)
        distance
        (let [tree (first distant-trees)]
          (if (>= tree target-tree)
            (inc distance)
            (recur (rest distant-trees) (inc distance))))))))

(defn get-scenic-score-of-tree [forest tree-coord]
  (let [rays (create-rays (bounds-of forest) tree-coord)]
    (reduce * (map #(get-scenic-score-of-ray forest %) rays))))

(defn find-best-scenic-score [file-name]
  (let [forest (parse-forest (slurp file-name))
        [w h] (bounds-of forest)
        scores (for [x (range w) y (range h)]
                 (get-scenic-score-of-tree forest [x y]))]
    (apply max scores)))