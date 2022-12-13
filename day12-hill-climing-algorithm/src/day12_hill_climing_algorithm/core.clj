(ns day12-hill-climing-algorithm.core
  (:require [clojure.string :as string]))

(defn get-cell [grid [x y]]
  (try
    (-> grid (nth y) (nth x))
    (catch Exception _e
      "TILT")))

(defn get-bounds [grid]
  [(count (first grid)) (count grid)])

(defn in-bounds [grid [x y]]
  (let [[bx by] (get-bounds grid)]
    (and
      (< -1 x bx)
      (< -1 y by))))

(defn ortho-from [grid [x y]]
  (let [orthos [[x (dec y)]
                [(inc x) y]
                [x (inc y)]
                [(dec x) y]]
        orthos (filter (partial in-bounds grid) orthos)]
    (set orthos)))

(defn parse-line [line]
  (let [line (replace {\S \a \E \z} line)
        line (map int line)
        line (map #(- % (int \a)) line)]
    line))

(defn find-in-lines [lines target]
  (let [found (for [y (range (count lines)) x (range (count (first lines)))]
                (if (= target (-> lines (nth y) (nth x)))
                  [x y]
                  nil))]
    (first (remove nil? found)))
  )

(defn parse-lines [lines]
  (let [grid (map parse-line lines)
        start (find-in-lines lines \S)
        end (find-in-lines lines \E)]
    [start end grid])
  )

(defn make-height-map [file-name]
  (let [lines (string/split-lines (slurp file-name))]
    (parse-lines lines)))

(defn valid-climbing-step? [grid height pos]
  (let [pos-height (get-cell grid pos)
        diff (- pos-height height)]
    (<= diff 1)))

(defn valid-descending-step? [grid height pos]
  (let [pos-height (get-cell grid pos)
        diff (- pos-height height)]
    (>= diff -1)))

(defn possible-steps-while-climbing [grid pos]
  (let [orthos (ortho-from grid pos)
        height (get-cell grid pos)
        steps (filter (partial valid-climbing-step? grid height) orthos)]
    (set steps)))

(defn possible-steps-while-descending [grid pos]
  (let [orthos (ortho-from grid pos)
        height (get-cell grid pos)
        steps (filter (partial valid-descending-step? grid height) orthos)]
    (set steps)))

(defn distance [p1 p2]
  (reduce + (map #(Math/abs ^int %) (map - p1 p2))))

(defn distance-3d [grid end step]
  (let [distance-to-end (distance step end)
        remaining-height (- 25 (get-cell grid step))]
    (if (>= distance-to-end remaining-height)
      distance-to-end
      (+ distance-to-end (- remaining-height distance-to-end)))))

(defn prune-and-prioritize [end grid path shortest steps]
  (let [steps (remove #(contains? (set path) %) steps)
        max-distance (- (count shortest) (count path))
        steps (remove #(> (distance % end) max-distance) steps)
        distance-and-steps (map #(vector (distance-3d grid end %) %) steps)
        sorted-by-distance (sort-by first distance-and-steps)]
    (map second sorted-by-distance)))

(defn prune-path [path steps]
  (let [steps (remove #(contains? (set path) %) steps)]
        steps))

(defn make-shortest-map [grid max-path-length]
  (let [[bx by] (get-bounds grid)
        cell-positions (for [x (range bx) y (range by)] [x y])
        shortest-map (apply hash-map (interleave cell-positions (repeat (* bx by) max-path-length)))]
    shortest-map))

(defn find-shortest-path
  ([[start end grid]]
   (let [shortest (atom (repeat 1000 0))
         shortest-map (atom (make-shortest-map grid 1000))]
     (find-shortest-path start end grid [start] shortest shortest-map)
     @shortest))

  ([pos end grid path shortest shortest-map]
   (let [path-length (count path)]
     (when (< path-length (get @shortest-map pos))
       (swap! shortest-map assoc pos path-length)
       (cond
         (= pos end)
         (do
           (prn path-length path)
           (reset! shortest path))

         (< path-length (count @shortest))
         (let [steps (possible-steps-while-climbing grid pos)
               prioritized-steps (prune-and-prioritize end grid path @shortest steps)]
           (doseq [step prioritized-steps]
             (find-shortest-path step end grid (conj path step) shortest shortest-map)))

         :else
         nil)))))

(defn shortest-path-length [file-name]
  (let [height-map (parse-lines (string/split-lines (slurp file-name)))
        shortest (find-shortest-path height-map)]
    (dec (count shortest))))

(defn find-shortest-path-to-low-ground
  ([start grid]
   (let [shortest (atom (repeat 1000 0))
         shortest-map (atom (make-shortest-map grid 1000))]
     (find-shortest-path-to-low-ground start grid [start] shortest shortest-map)
     @shortest))

  ([pos grid path shortest shortest-map]
   (let [path-length (count path)]
     (when (< path-length (get @shortest-map pos))
       (swap! shortest-map assoc pos path-length)
       (cond
         (zero? (get-cell grid pos))
         (do
           (prn path-length path)
           (reset! shortest path))

         (< path-length (count @shortest))
         (let [steps (possible-steps-while-descending grid pos)
               prioritized-steps (prune-path path steps)]
           (doseq [step prioritized-steps]
             (find-shortest-path-to-low-ground step grid (conj path step) shortest shortest-map)))

         :else
         nil)))))

(defn shortest-scenic-path [file-name]
  (let [[_ end grid] (parse-lines (string/split-lines (slurp file-name)))
        shortest (find-shortest-path-to-low-ground end grid)]
    (dec (count shortest))))
