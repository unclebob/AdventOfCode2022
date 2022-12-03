(ns day3-rucksack-reorganization.core
  (:require [clojure.string :as string]
            [clojure.set :as set]))

(defn priority [c]
  (if (Character/isUpperCase ^char c)
    (+ 27 (- (int c) (int \A)))
    (+ 1 (- (int c) (int \a))))
  )

(defn parse-rucksack [line]
  (let [priorities (map priority line)]
    (partition (/ (count priorities) 2) priorities)))

(defn parse-rucksack-file [file-name]
  (let [rucksacks (slurp file-name)
        lines (string/split-lines rucksacks)]
    (map parse-rucksack lines)))

(defn prioritize-rucksack-line [rucksack-line]
  (map priority rucksack-line))

(defn parse-groups [rucksack-lines]
  (let [groups (map prioritize-rucksack-line rucksack-lines)]
    (partition 3 groups)))

(defn common-item [rucksack]
  (let [[c1 c2] rucksack]
    (first (set/intersection (set c1) (set c2)))))

(defn common-items [rucksacks]
  (map common-item rucksacks))

(defn sum-of-common-items [file-name]
  (reduce + (common-items (parse-rucksack-file file-name))))

(defn find-badge [group]
  (let [[s1 s2 s3] (map set group)]
    (first (set/intersection s3 (set/intersection s1 s2))))
  )

(defn find-badges [groups]
  (map find-badge groups))

(defn sum-of-badges [file-name]
  (let [lines (string/split-lines (slurp file-name))
        groups (parse-groups lines)
        badges (find-badges groups)]
    (reduce + badges)))

