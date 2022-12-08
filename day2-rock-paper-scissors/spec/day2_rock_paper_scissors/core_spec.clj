(ns day2-rock-paper-scissors.core-spec
  (:require [speclj.core :refer :all]
            [day2-rock-paper-scissors.core :refer :all]))

(describe "parsing input"
  (it "should read assumed trategy"
    (should= [[:rock :paper] [:paper :rock] [:scissors :scissors]]
             (read-assumed-strategy "test-input")))

  (it "should read actual strategy"
    (should= [[:rock :rock] [:paper :rock] [:scissors :rock]]
             (read-actual-strategy "test-input"))))

(describe "Playing the assumed game"
  (it "should score the test game"
    (should= 15 (score-of-strategy [[:rock :paper] [:paper :rock] [:scissors :scissors]]))))

(describe "Playing the actual game"
  (it "should score the test game"
    (should= 12 (score-of-strategy [[:rock :rock] [:paper :rock] [:scissors :rock]]))))

(describe "solutions"
  (it "solves the assumed strategy (part 1)"
    (should= 10994 (play-assumed-strategy "input")))

  (it "solves the actual strategy (part 2)"
    (should= 12526 (play-actual-strategy "input"))))