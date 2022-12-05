(ns day5-supply-stacks.core
  (:require [clojure.string :as string]))

(defn separate-stacks-and-moves [input]
  (loop [lines (string/split-lines input)
         stacks []
         moves []
         type :stacks]
    (if (empty? lines)
      [stacks moves]
      (let [line (first lines)]
        (cond
          (empty? line) (recur (rest lines) stacks moves :moves)
          (= type :stacks) (recur (rest lines) (conj stacks line) moves :stacks)
          :else (recur (rest lines) stacks (conj moves line) :moves))))))

(defn determine-number-of-stacks [stack-lines]
  (dec (count (string/split (last stack-lines) #"\s+"))))

(defn break-into-columns [n-columns stack-line]
  (let [groups-of-4-chars (partition 4 4 " " stack-line)
        partial-columns (map #(str (nth % 1)) groups-of-4-chars)
        column-pad (repeat n-columns " ")]
    (take n-columns (concat partial-columns column-pad))))

(defn extract-stack-names [stack-lines]
  (let [n-stacks (determine-number-of-stacks stack-lines)
        stack-lines (drop-last stack-lines)
        columns (map (partial break-into-columns n-stacks) stack-lines)]
    columns))

(defn remove-spaces [stack-with-spaces]
  (remove #(= " " %) stack-with-spaces))

(defn build-stacks [stack-lines]
  (let [n-rows (dec (count stack-lines))
        stack-rows (extract-stack-names stack-lines)
        stacks-with-spaces (partition n-rows (apply interleave stack-rows))]
    (map remove-spaces stacks-with-spaces)))

(defn parse-move [move]
  (let [matches (re-matches #"move (\d+) from (\d+) to (\d+)" move)]
    (map #(Integer/parseInt %) (rest matches))))

(defn parse-moves [moves]
  (map parse-move moves))

(defn move-1 [stacks [from to]]
  (let [stacks (vec stacks)
        from-index (dec from)
        to-index (dec to)
        item (first (nth stacks from-index))
        stacks (update-in stacks [from-index] rest)
        stacks (update-in stacks [to-index] (partial concat [item]))]
    stacks))

(defn cm9000-mover [stacks move]
  (let [[n from to] move]
    (reduce move-1 stacks (repeat n [from to]))))

(defn execute-moves [mover stacks moves]
  (reduce mover stacks moves))

(defn tops-of-stacks [stacks]
  (apply str (map first stacks)))

(defn solve [mover file-name]
  (let [input (slurp file-name)
        [stack-lines move-lines] (separate-stacks-and-moves input)
        stacks (build-stacks stack-lines)
        moves (parse-moves move-lines)
        stacks (execute-moves mover stacks moves)]
    (tops-of-stacks stacks)))

(defn cm9001-mover [stacks [n from to]]
  (let [stacks (vec stacks)
        from-index (dec from)
        to-index (dec to)
        items (take n (nth stacks from-index))
        stacks (update-in stacks [from-index] (partial drop n))
        stacks (update-in stacks [to-index] (partial concat items))]
    stacks))
