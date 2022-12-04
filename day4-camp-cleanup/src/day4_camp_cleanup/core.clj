(ns day4-camp-cleanup.core
  (:require [clojure.string :as string]
            [clojure.set :as set]))

(defn range-to-sections [range-string]
  (let [[from to] (map #(Integer/parseInt %) (string/split range-string #"-"))]
    (set (range from (inc to)))))

(defn line-to-assignment [line]
  (let [ranges (string/split line #",")]
    (map range-to-sections ranges)))

(defn create-pair-assignments [file-content]
  (map line-to-assignment (string/split-lines file-content)))

(defn find-overlap [pair-assignment]
  (let [[e1 e2] pair-assignment]
    (set/intersection e1 e2)))

(defn is-full-overlap? [pair-assignment]
  (let [[e1 e2] pair-assignment
        overlap (find-overlap pair-assignment)
        overlap-count (count overlap)]
    (or (= overlap-count (count e1))
        (= overlap-count (count e2)))))

(defn count-full-overlaps [file-name]
  (let [pair-assignments (create-pair-assignments (slurp file-name))]
    (count (filter is-full-overlap? pair-assignments))))

(defn overlaps? [pair-assignment]
  (seq (find-overlap pair-assignment)))

(defn count-overlapping-pairs [file-name]
  (let [pair-assignments (create-pair-assignments (slurp file-name))]
      (count (filter overlaps? pair-assignments))))

