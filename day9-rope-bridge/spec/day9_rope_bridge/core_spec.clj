(ns day9-rope-bridge.core-spec
  (:require [speclj.core :refer :all]
            [day9-rope-bridge.core :refer :all]))

(describe "parse move directions"
  (it "does it"
    (should= [:right :right :right :right] (parse-move "R 4"))
    (should= [:left :left] (parse-move "L 2"))
    (should= [:up] (parse-move "U 1"))
    (should= [:down :down :down] (parse-move "D 3"))))

(describe "move pairs of knots"
  (it "moves right"
    (should= [[3 2] [2 2]] (move-pair [1 0] [2 2] [1 1])))
  (it "moves left"
    (should= [[1 2] [2 2]] (move-pair [-1 0] [2 2] [2 2])))
  (it "moves up"
    (should= [[2 3] [2 2]] (move-pair [0 1] [2 2] [2 2])))
  (it "moves down"
    (should= [[2 1] [2 2]] (move-pair [0 -1] [2 2] [2 2])))
  (it "moves up-right"
    (should= [[1 2] [1 1]] (move-pair [1 1] [0 1] [0 0]))))

(describe "move a rope"
  (it "should unroll"
    (should= [[1 0] [0 0] [0 0]] (move-rope [1 0] [[0 0] [0 0] [0 0]]))
    (should= [[2 0] [1 0] [0 0]] (move-rope [1 0] [[1 0] [0 0] [0 0]]))
    (should= [[3 0] [2 0] [1 0]] (move-rope [1 0] [[2 0] [1 0] [0 0]]))
    (should= [[3 1] [2 0] [1 0]] (move-rope [0 1] [[3 0] [2 0] [1 0]]))
    (should= [[3 2] [3 1] [2 1] [1 1]] (move-rope [0 1] [[3 1] [2 0] [1 0] [0 0]]))))

(describe "part 1 solution"
  (it "solves test input"
    (should= 13 (count (track-tail-of-rope "test-input" 2))))
  (it "solves input"
    (should= 6081 (count (track-tail-of-rope "input" 2)))))

(describe "Part 2 solution, a rope of 9 knots"
  (it "solves the test input"
    (should= 36 (count (track-tail-of-rope "test-input-2" 10))))

  (it "solves the test input"
      (should= 2487 (count (track-tail-of-rope "input" 10)))))
