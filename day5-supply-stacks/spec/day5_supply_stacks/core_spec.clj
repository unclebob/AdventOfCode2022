(ns day5-supply-stacks.core-spec
  (:require [speclj.core :refer :all]
            [day5-supply-stacks.core :refer :all]))

(describe "parsing input"
  (it "separates stack map from moves"
    (should= [["stack" "map"] ["move" "directions"]]
             (separate-stacks-and-moves "stack\nmap\n\nmove\ndirections")))

  (it "determines number of stacks by counting last line"
    (should= 3 (determine-number-of-stacks ["one two" " 1   2   3  "])))

  (it "extracts stack names"
    (should= [[" " "A" " "] ["B" "C" "D"]]
             (extract-stack-names
               ["    [A]"
                "[B] [C] [D]"
                " 1   2   3"])))

  (it "creates stacks from stack lines"
    (should= [["B"] ["A" "C"] ["D"]]
             (build-stacks ["    [A]"
                            "[B] [C] [D]"
                            " 1   2   3"])))

  (it "parses a move"
    (should= [1 2 3] (parse-move "move 1 from 2 to 3")))

  (it "parses moves"
    (should= [[1 2 3] [4 5 6]] (parse-moves ["move 1 from 2 to 3"
                                             "move 4 from 5 to 6"]))))

(describe "Part one utilties"
  (it "moves one item"
      (should= [["B"] ["A" "C"]] (move-1 [["A" "B"] ["C"]] [1 2])))

  (it "executes one true move"
    (should= [[] ["B" "A" "C"]] (cm9000-mover [["A" "B"] ["C"]] [2 1 2])))

  (it "returns top of stacks"
    (should= "CMZ" (tops-of-stacks [["C" "A"] ["M"] ["Z" "S"]])))
  )

(describe "Solution of part one"
  (it "should solve test input"
    (should= "CMZ" (solve cm9000-mover "test-input")))

  (it "should solve input"
    (should= "NTWZZWHFV" (solve cm9000-mover "input"))))

(describe "Part 2 utilities"
  (it "executes a Cratemover 9001 move"
    (should= [[] ["A" "B" "C"]] (cm9001-mover  [["A" "B"] ["C"]] [2 1 2]))))

(describe "Solution of part 2"
  (it "solves test input"
    (should= "MCD" (solve cm9001-mover "test-input")))

  (it "solves input"
    (should= "" (solve cm9001-mover "input"))))