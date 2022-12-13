(ns day12-hill-climing-algorithm.core-spec
  (:require [speclj.core :refer :all]
            [day12-hill-climing-algorithm.core :refer :all]))

(declare grid)
(declare hm start end)

(describe "day 12 Hill Climbing Algorithm"
  (context "grid manipulations"
    (with grid [[1 2 3 -1]
                [4 5 6 -2]
                [7 8 9 -3]])

    (it "translates coordinates to heights"
      (should= 1 (get-cell @grid [0 0]))
      (should= 2 (get-cell @grid [1 0]))
      (should= 9 (get-cell @grid [2 2]))
      (should= "TILT" (get-cell @grid [3 3])))

    (it "gets bounds of grid"
      (should= [4 3] (get-bounds @grid)))

    (it "gets orthogonal steps"
      (should= #{[1 0] [2 1] [1 2] [0 1]}
               (ortho-from @grid [1 1]))
      (should= #{[1 0] [0 1]}
               (ortho-from @grid [0 0]))
      (should= #{[3 1] [2 2]}
               (ortho-from @grid [3 2]))))

  (context "parsing input"
    (it "creates the heightmap from lines"
      (should= [[2 0] [2 1]
                [[0 1 0 3]
                 [2 1 25 5]
                 [16 17 18 19]]]
               (parse-lines ["abSd"
                             "cbEf"
                             "qrst"])))
    (it "parses a file"
      (let [hm (make-height-map "test-input")]
        (should= [[0 0] [5 2]
                  '((0 0 1 16 15 14 13 12)
                    (0 1 2 17 24 23 23 11)
                    (0 2 2 18 25 25 23 10)
                    (0 2 2 19 20 21 22 9)
                    (0 1 3 4 5 6 7 8))]
                 hm))))

  ;test-input
  ;Sabqponm
  ;abcryxxl
  ;accszExk
  ;acctuvwj
  ;abdefghi

  (context "path manipulations"
    (with hm (make-height-map "test-input"))
    (with grid (nth @hm 2))
    (with start (first @hm))
    (with end (second @hm))
    (it "finds possible steps"
      (should= #{[1 0] [0 1]}
               (possible-steps-while-climbing @grid @start))
      (should= #{[4 2] [5 1] [3 1] [4 0]}
               (possible-steps-while-climbing @grid [4 1]))
      (should= #{[4 3] [5 4] [6 3]}
               (possible-steps-while-climbing @grid [5 3])))

    (it "calculates orthogonal distance"
      (should= 0 (distance [1 1] [1 1]))
      (should= 1 (distance [1 1] [1 0]))
      (should= 1 (distance [0 1] [1 1]))
      (should= 2 (distance [0 0] [1 1])))

    (it "makes shortest map"
      (should= {[0 0] 1000, [1 0] 1000, [1 1] 1000, [0 1] 1000}
               (make-shortest-map [[1 2] [3 4]] 1000))))

  (context "part 1 solution"
    (it "solves test data"
      (should= 31 (shortest-path-length "test-input")))

    (it "solves input"
      (should= 408 (shortest-path-length "input"))))

  (context "part 2 solution"
    (it "solves test data"
      (should= 29 (shortest-scenic-path "test-input")))

    (it "solves input"
      (should= 399 (shortest-scenic-path "input")))))
