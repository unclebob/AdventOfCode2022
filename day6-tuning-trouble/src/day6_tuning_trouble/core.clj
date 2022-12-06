(ns day6-tuning-trouble.core
  (:require [clojure.string :as string]))

(defn extract-frames [stream]
  (let [s1 stream
        s2 (drop 1 stream)
        s3 (drop 1 s2)
        s4 (drop 1 s3)]
    (map str s1 s2 s3 s4))
  )

(defn is-packet-start? [frame]
  (= 4 (count (set frame))))

(defn get-first-packet-start [stream]
  (let [frames (extract-frames stream)
        starts (filter is-packet-start? frames)]
    (first starts)))

(defn get-packet-start-position [stream]
  (let [first-start (get-first-packet-start stream)]
    (+ 4 (string/index-of stream first-start))))

(defn extract-message-frames [stream]
  (let [s1 stream
        s2 (drop 1 stream)
        s3 (drop 1 s2)
        s4 (drop 1 s3)
        s5 (drop 1 s4)
        s6 (drop 1 s5)
        s7 (drop 1 s6)
        s8 (drop 1 s7)
        s9 (drop 1 s8)
        s10 (drop 1 s9)
        s11 (drop 1 s10)
        s12 (drop 1 s11)
        s13 (drop 1 s12)
        s14 (drop 1 s13)]
    (map str s1 s2 s3 s4 s5 s6 s7 s8 s9 s10 s11 s12 s13 s14))
  )

(defn is-message-start? [message-frame]
  (= 14 (count (set message-frame))))

(defn get-first-message-start [stream]
  (let [frames (extract-message-frames stream)
        starts (filter is-message-start? frames)]
    (first starts)))

(defn get-message-start-position [stream]
  (let [first-start (get-first-message-start stream)]
    (+ 14 (string/index-of stream first-start))))