(ns day2-rock-paper-scissors.core
  (:require [clojure.string :as string]))

(def assumed-translation
  {
   "A" :rock
   "B" :paper
   "C" :scissors
   "X" :rock
   "Y" :paper
   "Z" :scissors
   })

(def actual-translation
  {
   "A" :rock
   "B" :paper
   "C" :scissors
   "X" :lose
   "Y" :draw
   "Z" :win
   })

(def score
  {:rock 1
   :paper 2
   :scissors 3})

(def wins
  #{[:rock :paper]
    [:paper :scissors]
    [:scissors :rock]
    })

(def to-lose
  {:rock :scissors
  :paper :rock
  :scissors :paper})

(def to-win
  {:rock :paper
  :paper :scissors
  :scissors :rock})

(defn read-strategy [file-name translation]
  (let [input (slurp file-name)
          lines (string/split-lines input)]
      (for [line lines]
        (let [[opponent player] (string/split line #" ")]
          (map translation [opponent player])))))

(defn read-assumed-strategy [file-name]
  (read-strategy file-name assumed-translation))

(defn choose-response [[opponent outcome]]
  (condp = outcome
    :win [opponent (to-win opponent)]
    :draw [opponent opponent]
    :lose [opponent (to-lose opponent)]))

(defn read-actual-strategy [file-name]
  (let [strategy (read-strategy file-name actual-translation)]
    (map choose-response strategy)))

(defn outcome-score [round]
  (cond
    (wins round) 6
    (apply = round) 3
    :else 0))

(defn score-of-round [round]
  (let [choice-score (score (second round))]
    (+ choice-score (outcome-score round))))

(defn score-of-strategy [rounds]
  (let [scores (map score-of-round rounds)]
    (reduce + scores)))

(defn play-assumed-strategy [file-name]
  (score-of-strategy (read-assumed-strategy file-name)))

(defn play-actual-strategy [file-name]
  (score-of-strategy (read-actual-strategy file-name)))


