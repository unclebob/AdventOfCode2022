(ns day14-regolith-reservoir.core-spec
  (:require [speclj.core :refer :all]
            [day14-regolith-reservoir.core :refer :all]))

;right and down are positive.

(describe "Day 14 regolith reservoir"
  (context "Sand mechanics"
    (it "falls down if nothing in the way"
      (should= [0 1] (sand-falls-one-step #{} [0 0])))
    (it "falls diagonally left if blocked below"
      (should= [-1 1] (sand-falls-one-step #{[0 1]} [0 0])))
    (it "falls diagonally right if blocked below and left"
      (should= [1 1] (sand-falls-one-step #{[0 1] [-1 1]} [0 0])))
    (it "does not fall if blocked below, left, and right"
      (should= nil (sand-falls-one-step #{[0 1] [-1 1] [1 1]} [0 0])))
    (it "falls until stopped"
      (should= [0 2] (drop-sand #{[-1 3] [0 3] [1 3]} [0 0]))
      (should= [-1 3] (drop-sand #{[0 3] [1 3] [-2 4] [-1 4] [0 4]} [0 0])))
    (it "falls into the void"
      (should= :void (drop-sand #{[0 3]} [0 0])))
    (it "adds sand to cave"
      (should= [[0 1] #{[-1 2] [0 2] [1 2] [0 1]}]
               (add-sand #{[-1 2] [0 2] [1 2]} [0 0])))
    (it "drops until void"
      (should= [4 #{[-2 3] [2 3] [-1 3] [-1 2] [1 3] [0 3] [0 2] [1 2] [0 1]}]
               (drop-til-target #{[-2 3] [-1 3] [0 3] [1 3] [2 3]} [0 0] :void))))

  (context "parsing the input"
    (it "parses a coordinate"
      (should= [2 3] (parse-coord "2,3")))
    (it "parses the wall vertices"
      (should= [[1 2] [3 4] [5 6]]
               (parse-wall-vertices "1,2 -> 3,4 -> 5,6"))))

  (context "building walls"
    (it "builds one wall segement"
      (should= #{[1 1] [1 2] [1 3]} (build-segment [1 1] [1 3]))
      (should= #{[1 1] [1 2] [1 3]} (build-segment [1 3] [1 1]))
      (should= #{[1 1] [2 1] [3 1]} (build-segment [1 1] [3 1]))
      (should= #{[1 1] [2 1] [3 1]} (build-segment [3 1] [1 1])))

    (it "builds a whole wall"
      (should= #{[1 1] [1 2] [1 3] [2 3] [3 3]}
               (build-wall [[1 1] [1 3] [3 3]]))))

  (context "Part 1 solution"
    (it "solves test-input"
      (should= 24 (count-remaining-sand "test-input")))
    (it "solves input"
      (should= 698 (count-remaining-sand "input"))))

  (context "Part 2 solution"
    (it "solves test input"
      (should= 93 (count-til-blocked "test-input")))
    (it "solves input"
      (should= 28594 (count-til-blocked "input"))))
  )
