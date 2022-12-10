(ns day10-cathode-ray-tube.core
  (:require [clojure.string :as string]))


(defn noop [state]
  (let [{:keys [x cycles]} state
        cycles (conj cycles {:x x})]
    {:x x :cycles cycles})
  )

(defn addx [n state]
  (let [{:keys [x cycles]} state
        cycles (conj cycles {:x x})
        cycles (conj cycles {:x x})]
    {:x (+ x n) :cycles cycles}))

(defn execute [state lines]
  (if (empty? lines)
    state
    (let [line (first lines)
          state (cond (re-matches #"noop" line)
                      (noop state)

                      (re-matches #"addx (-?\d+)" line)
                      (let [[_ n] (re-matches #"addx (-?\d+)" line)
                            n (Integer/parseInt n)]
                        (addx n state))

                      :else
                      "TILT")]
      (recur state (rest lines)))))


(defn signal-strength [file-name]
  (let [lines (string/split-lines (slurp file-name))
        state (execute {:x 1 :cycles []} lines)
        {:keys [x cycles]} state
        strengths (for [cycle [20 60 100 140 180 220]]
                    (* cycle (:x (nth cycles (dec cycle)))))]
    (reduce + strengths)))

(defn render [cycles]
  (loop [cycles cycles
         screen ""
         t 0]
    (if (empty? cycles)
      (map #(apply str %) (partition 40 40 "" screen))
      (let [cycle (first cycles)
            offset (- t (:x cycle))
            pixel? (<= -1 offset 1)
            screen (str screen (if pixel? "#" "."))
            t (mod (inc t) 40)]
        (recur (rest cycles) screen t)))))

(defn execution-of [file-name]
  (let [lines (string/split-lines (slurp file-name))
        {:keys [cycles]} (execute {:x 1 :cycles []} lines)]
    cycles))

(defn print-screen [lines]
  (doseq [line lines]
    (println line))
  true)