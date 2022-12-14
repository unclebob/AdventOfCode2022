(ns day13-distress-signal.core-spec
  (:require [speclj.core :refer :all]
            [day13-distress-signal.core :refer :all]))

(describe "distress signal"
  (context "comparisons"
    (it "compares two integers"
      (should= :right-order (compare-signal 1 2))
      (should= :wrong-order (compare-signal 2 1))
      (should= :equal (compare-signal 1 1)))

    (it "compares two lists"
      (should= :equal (compare-signal [1] [1]))
      (should= :right-order (compare-signal [1] [2]))
      (should= :right-order (compare-signal [1] [1 1]))
      (should= :right-order (compare-signal [1 1] [1 2]))
      (should= :right-order (compare-signal [1 [2 1]] [1 [2 3]]))
      (should= :right-order (compare-signal [1 [2 1]] [1 [2 1 1]]))
      (should= :right-order (compare-signal [] [[] []]))
      (should= :right-order (compare-signal [[]] [[] []]))


      (should= :wrong-order (compare-signal [2] [1]))
      (should= :wrong-order (compare-signal [1 1] [1]))
      (should= :wrong-order (compare-signal [1 [2 3]] [1 [2 1]]))
      (should= :wrong-order (compare-signal [1 [2 1 1]] [1 [2 1]])))

    (it "compares and integer and a list"
      (should= :right-order (compare-signal 1 [2]))
      (should= :wrong-order (compare-signal [2] 1)))

    (it "compares sample data correctly"
      (should= :right-order (compare-signal [1, 1, 3, 1, 1] [1, 1, 5, 1, 1]))
      (should= :right-order (compare-signal [[1], [2, 3, 4]] [[1], 4]))
      (should= :right-order (compare-signal [[4, 4], 4, 4] [[4, 4], 4, 4, 4]))
      (should= :right-order (compare-signal [] [3]))

      (should= :wrong-order (compare-signal [9] [[8, 7, 6]]))
      (should= :wrong-order (compare-signal [7, 7, 7, 7] [7, 7, 7]))
      (should= :wrong-order (compare-signal [[[]]] [[]]))
      (should= :wrong-order (compare-signal [1, [2, [3, [4, [5, 6, 7]]]], 8, 9] [1, [2, [3, [4, [5, 6, 0]]]], 8, 9]))))

  (context "parsing data"
    (it "parses pairs"
      (should= [1 2] (parse-pair "1" "2"))
      (should= [[1] [2]] (parse-pair "[1]" "[2]"))
      (should= [[1, [2, [3, [4, [5, 6, 7]]]], 8, 9]
                [1, [2, [3, [4, [5, 6, 0]]]], 8, 9]]
               (parse-pair "[1,[2,[3,[4,[5,6,7]]]],8,9]"
                           "[1,[2,[3,[4,[5,6,0]]]],8,9]")))

    (it "creates pair map"
      (should= {1 [1 2]
                2 [3 4]}
               (make-pair-map ["1" "2" "" "3" "4"]))))

  (context "part 1 solution"
    (it "solves test data"
      (should= 13 (sum-right-indices "test-input")))

    (it "solves input"
      (should= 6187 (sum-right-indices "input"))))

  (context "part 2 solution"
    (it "sorts all the packets"
      (should= [
                []
                [[]]
                [[[]]]
                [1, 1, 3, 1, 1]
                [1, 1, 5, 1, 1]
                [[1], [2, 3, 4]]
                [1, [2, [3, [4, [5, 6, 0]]]], 8, 9]
                [1, [2, [3, [4, [5, 6, 7]]]], 8, 9]
                [[1], 4]
                [[2]]
                [3]
                [[4, 4], 4, 4]
                [[4, 4], 4, 4, 4]
                [[6]]
                [7, 7, 7]
                [7, 7, 7, 7]
                [[8, 7, 6]]
                [9]
                ] (sort-packets [
                                 [1, 1, 3, 1, 1]
                                 [1, 1, 5, 1, 1]
                                 [[1], [2, 3, 4]]
                                 [[1], 4]
                                 [9]
                                 [[8, 7, 6]]
                                 [[4, 4], 4, 4]
                                 [[4, 4], 4, 4, 4]
                                 [7, 7, 7, 7]
                                 [7, 7, 7]
                                 []
                                 [3]
                                 [[[]]]
                                 [[]]
                                 [1, [2, [3, [4, [5, 6, 7]]]], 8, 9]
                                 [1, [2, [3, [4, [5, 6, 0]]]], 8, 9]
                                 [[2]]
                                 [[6]]
                                 ])))
    )

  (it "solves test-input"
    (should= 140 (decoder-key-of "test-input")))

  (it "solves input data"
    (should= 23520 (decoder-key-of "input"))))


