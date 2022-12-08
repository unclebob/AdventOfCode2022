(ns day8-treetop-tree-house.core-spec
  (:require [speclj.core :refer :all]
            [day8-treetop-tree-house.core :refer :all]))

(describe "parsing the forest file"
  (it "parses nothing"
    (should= [[]] (parse-forest "")))

  (it "parses a simple forest"
    (should= [[1 2] [3 4]] (parse-forest "12\n34")))
  )

(describe "part 1 utilities -- finding hidden trees"
  (it "counts trees"
    (should= 9 (count-trees [[1 1 1] [1 1 1] [1 1 1]])))
  (it "rotates the forest"
    (should= [[1 3]
              [2 4]] (rotate-forest [[1 2]
                                     [3 4]])))

  (it "finds hidden trees in a row"
    (should= #{1 2} (hidden-from-left [3 2 3 5]))
    (should= #{0 1 2} (hidden-from-right [3 1 2 5]))
    (should= #{1 2} (hidden-in-row [3 1 2 5]))
    (should= #{} (hidden-in-row [1 5 1])))

  (it "finds hidden trees"
    (should= #{[1 0] [1 1] [1 2]}
             (find-hidden-trees-by-row [[1 1 1]
                                        [1 0 1]
                                        [1 1 1]]))
    (should= #{[0 1] [1 1] [2 1]}
             (find-hidden-trees-by-column [[1 1 1]
                                           [1 0 1]
                                           [1 1 1]]))

    (should= #{[1 1]} (find-hidden-trees [[1 1 1]
                                          [1 0 1]
                                          [1 1 1]]))))

(describe "part 1 solution"
  (it "should solve test-input"
    (should= 21 (count-visible-trees "test-input")))

  (it "should solve input"
    (should= 1803 (count-visible-trees "input"))))

(describe "Part 2 utilities, measuring visibility"
  (it "creates four rays"
    (should= #{[[2 1] [2 2] [2 3] [2 4]]
               [[2 1] [2 0]]
               [[2 1] [3 1] [4 1]]
               [[2 1] [1 1] [0 1]]} (create-rays [5 5] [2 1]))
    (should= #{[[0 0]]
               [[0 0] [1 0] [2 0] [3 0] [4 0]]
               [[0 0] [0 1] [0 2] [0 3] [0 4]]}
             (create-rays [5 5] [0 0]))
    (should= #{[[4 4]]
               [[4 4] [3 4] [2 4] [1 4] [0 4]]
               [[4 4] [4 3] [4 2] [4 1] [4 0]]}
             (create-rays [5 5] [4 4])))

  (it "gets a tree from a coordinate"
    (let [forest [[1 2 3]
                 [4 5 6]
                 [7 8 9]]]
      (should= 1 (get-tree forest [0 0]))
      (should= 5 (get-tree forest [1 1]))
      (should= 9 (get-tree forest [2 2]))))

  (it "calculates scenic score of ray"
    (let [forest (parse-forest (slurp "test-input"))]
      (should= 2 (get-scenic-score-of-ray forest [[2 3] [2 2] [2 1] [2 0]]))
      (should= 2 (get-scenic-score-of-ray forest [[2 3] [1 3] [0 3]]))
      (should= 1 (get-scenic-score-of-ray forest [[2 3] [2 4]]))
      (should= 2 (get-scenic-score-of-ray forest [[2 3] [3 3] [4 3]]))))

  (it "calculates scenic score of tree"
    (let [forest (parse-forest (slurp "test-input"))]
      (should= 8 (get-scenic-score-of-tree forest [2 3])))))

(describe "Part 2 solution"
  (it "solves test data"
    (should= 8 (find-best-scenic-score "test-input")))

  (it "solves input"
      (should= 268912 (find-best-scenic-score "input"))))


