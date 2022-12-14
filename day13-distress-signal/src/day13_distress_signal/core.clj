(ns day13-distress-signal.core
  (:require [clojure.string :as string]))

(defn compare-signal [a b]
  (cond
    (and (coll? a) (coll? b))
    (cond
      (and (empty? a) (empty? b))
      :equal

      (empty? a)
      :right-order

      (empty? b)
      :wrong-order

      :else
      (let [result (compare-signal (first a) (first b))]
        (if (= result :equal)
          (recur (rest a) (rest b))
          result)))

    (and (integer? a) (integer? b))
    (cond
      (< a b) :right-order
      (> a b) :wrong-order
      :else :equal)

    (and (integer? a) (coll? b))
    (compare-signal (vector a) b)

    (and (integer? b) (coll? a))
    (compare-signal a (vector b))))

(defn parse-pair [line1 line2]
  [(read-string line1) (read-string line2)])

(defn make-pair-map [lines]
  (loop [lines (remove empty? lines)
         pair-map {}
         index 1]
    (if (empty? lines)
      pair-map
      (let [pair (parse-pair (first lines) (second lines))]
        (recur (drop 2 lines) (assoc pair-map index pair) (inc index))))))

(defn sum-right-indices [file-name]
  (let [lines (string/split-lines (slurp file-name))
        pair-map (make-pair-map lines)]
    (loop [indices (keys pair-map)
           sum 0]
      (if (empty? indices)
        sum
        (let [index (first indices)]
          (if (= :right-order (apply compare-signal (pair-map index)))
            (recur (rest indices) (+ sum index))
            (recur (rest indices) sum)))))))

(defn order-comparator [a b]
  (let [order (compare-signal a b)]
    (condp = order
      :right-order -1
      :wrong-order 1
      :equal 0
      nil)))

(defn sort-packets [packets]
  (sort order-comparator packets))

(defn decoder-key-of [file-name]
  (let [lines (string/split-lines (slurp file-name))
        pair-map (make-pair-map lines)
        pairs (vals pair-map)
        packets (concat (map first pairs) (map second pairs) [[[2]] [[6]]])]
    (loop [packets (sort-packets packets)
           product 1
           index 1]
      (if (empty? packets)
        product
        (if (or (= (first packets) [[2]])
                (= (first packets) [[6]]))
          (recur (rest packets) (* index product) (inc index))
          (recur (rest packets) product (inc index)))))))