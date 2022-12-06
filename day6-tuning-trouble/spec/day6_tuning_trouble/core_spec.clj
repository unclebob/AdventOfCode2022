(ns day6-tuning-trouble.core-spec
  (:require [speclj.core :refer :all]
            [day6-tuning-trouble.core :refer :all]))

(describe "frames"
  (it "extract frames from a stream"
    (should= ["abcd" "bcde" "cdef"]
             (extract-frames "abcdef")))

  (it "should idenfity start of packet"
    (should-not (is-packet-start? "abab"))
    (should (is-packet-start? "abcd")))

  (it "should find first start of packet frame"
    (should= "jpqm" (get-first-packet-start "mjqjpqmgbljsphdztnvjfqwrcgsmlb")))

  (it "should find location of first packet start"
    (should= 7 (get-packet-start-position "mjqjpqmgbljsphdztnvjfqwrcgsmlb"))
    (should= 5 (get-packet-start-position "bvwbjplbgvbhsrlpgdmjqwftvncz"))
    (should= 6 (get-packet-start-position "nppdvjthqldpwncqszvftbrmjlhg"))
    (should= 10 (get-packet-start-position "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg"))
    (should= 11 (get-packet-start-position "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"))
    )
  )

(describe "part 1 solution"
  (it "solves input"
    (should= 1757 (get-packet-start-position (slurp "input")))))

(describe "part 2 solution"
  (it "solves test samples"
    (should= 19 (get-message-start-position "mjqjpqmgbljsphdztnvjfqwrcgsmlb"))
    (should= 23 (get-message-start-position "bvwbjplbgvbhsrlpgdmjqwftvncz"))
    (should= 23 (get-message-start-position "nppdvjthqldpwncqszvftbrmjlhg"))
    (should= 29 (get-message-start-position "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg"))
    (should= 26 (get-message-start-position "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw")))

  (it "sovles input"
    (should= 2950 (get-message-start-position (slurp "input"))))
  )