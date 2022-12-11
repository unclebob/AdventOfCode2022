(ns day11-monkey-in-the-middle.core
  (:require [clojure.string :as string]))

(defn replace-old [old rule-token]
  (if (= rule-token :old) old rule-token))

(defn inspect [item worry-reduction rule]
  (let [rule (map (partial replace-old item) rule)]
    (worry-reduction (apply (first rule) (rest rule)))))

(defn decide-where-to-throw [item monkey]
  (let [[divisor a b] (:decision monkey)]
    (if (= 0 (rem item divisor)) a b)))

(defn throw-one-item [monkeys worry-reduction monkey-id]
  (let [monkey (monkeys monkey-id)
        items (:items monkey)
        monkey (assoc monkey :items (vec (rest items)))
        monkey (update monkey :inspections inc)
        item (first items)
        item (inspect item worry-reduction (:rule monkey))
        recipient-id (decide-where-to-throw item monkey)
        monkeys (assoc monkeys monkey-id monkey)]
    (update-in monkeys [recipient-id :items] conj item)))

(defn do-one-monkey [worry-reduction monkeys monkey-id]
  (let [monkey (monkeys monkey-id)]
    (if (empty? (:items monkey))
      monkeys
      (recur worry-reduction (throw-one-item monkeys worry-reduction monkey-id) monkey-id))))

(defn do-one-round [monkeys worry-reduction]
  (reduce (partial do-one-monkey worry-reduction) monkeys (sort (keys monkeys))))

(defn parse-monkey [monkey-batch]
  (let [[_ id] (re-matches #"Monkey (\d+):" (first monkey-batch))
        id (Integer/parseInt id)
        [_ items] (re-matches #".*Starting items: (.*)$" (nth monkey-batch 1))
        items (string/split items #", ")
        items (vec (map #(Integer/parseInt %) items))
        [_ operation] (re-matches #".*Operation: new = (.*)$" (nth monkey-batch 2))
        [a op b] (string/split operation #" ")
        a (if (= a "old") :old (Integer/parseInt a))
        b (if (= b "old") :old (Integer/parseInt b))
        op (condp = op "*" * "+" + "TILT")
        [_ divisor] (re-matches #".*Test: divisible by (\d+)$" (nth monkey-batch 3))
        divisor (Integer/parseInt divisor)
        [_ m-true] (re-matches #".*If true: throw to monkey (\d+)$" (nth monkey-batch 4))
        m-true (Integer/parseInt m-true)
        [_ m-false] (re-matches #".*If false: throw to monkey (\d+)$" (nth monkey-batch 5))
        m-false (Integer/parseInt m-false)]
    [id {:items items
         :rule [op a b]
         :decision [divisor m-true m-false]
         :inspections 0}]))

(defn parse-monkeys [file-name]
  (let [lines (string/split-lines (slurp file-name))]
    (loop [batches (partition 7 7 "" lines)
           monkeys {}]
      (if (empty? batches)
        monkeys
        (let [[id monkey] (parse-monkey (first batches))]
          (recur (rest batches) (assoc monkeys id monkey)))))))

(defn do-n-rounds [monkeys n worry-reduction]
  (let [monkeys (reduce (fn [monkeys _] (do-one-round monkeys worry-reduction)) monkeys (range n))]
    monkeys))

(defn find-mode [monkeys]
  (let [divisors (map #(first (:decision %)) (vals monkeys))]
    (reduce * divisors)))

(defn monkey-business [file-name worry-reduction rounds]
  (let [monkeys (parse-monkeys file-name)
        final-monkeys (do-n-rounds monkeys rounds worry-reduction)
        inspections (sort (map :inspections (vals final-monkeys)))]
    (apply * (take-last 2 inspections))))

