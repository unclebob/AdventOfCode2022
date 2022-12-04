(ns day4-camp-cleanup.core-spec
  (:require [speclj.core :refer :all]
            [day4-camp-cleanup.core :refer :all]))

(describe "parsing input"
  (it "creates a section set from a range"
    (should= #{3 4 5} (range-to-sections "3-5")))

  (it "creates a pair assignment from a line"
    (should= [#{5 6 7 8} #{2 3 4}]
             (line-to-assignment "5-8,2-4")))

  (it "creates list of pair assignments"
    (should= [[#{1 2 3} #{4 5 6}]
              [#{7 8 9} #{10 11 12}]]
             (create-pair-assignments "1-3,4-6\n7-9,10-12"))))

(describe "part 1 utilities"
  (it "finds overlap of pair assignment"
    (should= #{3 4} (find-overlap [#{1 2 3 4} #{3 4 5 6}])))

  (it "detects full overlap"
    (should (is-full-overlap? [#{1 2 3 4} #{2 3}]))
    (should-not (is-full-overlap? [#{1 2 3 4} #{3 4 5 6}]))))

(describe "part 1 solution, count full overlaps in assignment pairs"
  (it "passes test data"
    (should= 2 (count-full-overlaps "test-input")))

  (it "solves input"
    (should= 487 (count-full-overlaps "input"))))

(describe "part 2 utilties"
  (it "finds any overlap"
    (should (overlaps? [#{1 2 3} #{3 4 5}]))
    (should-not (overlaps? [#{1 2 3} #{4 5 6}]))))

(describe "part 2 solution, count all overlapping pairs"
  (it "solves test data"
    (should= 4 (count-overlapping-pairs "test-input")))

  (it "solves input"
    (should= 849 (count-overlapping-pairs "input"))))
