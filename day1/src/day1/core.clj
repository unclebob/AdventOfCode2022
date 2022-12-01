(ns day1.core
  (:require [clojure.string :as string]))

(defn parse-inventory [file-name]
  (loop [lines (string/split-lines (slurp file-name))
         input [[]]
         elf 0]
    (if (empty? lines)
      input
      (let [line (first lines)]
        (if (empty? line)
          (recur (rest lines) (conj input []) (inc elf))
          (let [calories (Integer/parseInt line)]
            (recur (rest lines) (update-in input [elf] conj calories) elf)))))))

(defn sum-calories [inventory]
  (map #(reduce + %) inventory))

(defn max-calories [calorie-sums]
  (apply max calorie-sums))

(defn find-max-calories [input-file]
  (let [inventory (parse-inventory input-file)
        calorie-sums (sum-calories inventory)]
    (max-calories calorie-sums)))

(defn find-calories-in-top-three-elves [input-file]
  (let [inventory (parse-inventory input-file)
        sorted-calorie-sums (sort (sum-calories inventory))
        three-greatest (take-last 3 sorted-calorie-sums)]
    (reduce + three-greatest)))