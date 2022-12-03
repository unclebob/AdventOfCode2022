(ns day3-rucksack-reorganization.core-spec
  (:require [speclj.core :refer :all]
            [day3-rucksack-reorganization.core :refer :all]
            [clojure.string :as string]))

(describe "Parsing Rucksacks"
  (it "parses a rucksack"
    (should= [[1 26] [27 52]]
             (parse-rucksack "azAZ")))

  (it "parses many rucksacks"
    (should= (quote (((22 36 18 23 16 49 20 23 36 7 49 18) (8 3 19 32 39 39 6 32 32 8 32 16)) ((10 17 34 44 40 17 44 10 17 26 10 33 30 38 33 38) (18 19 32 39 6 32 52 45 18 38 18 32 52 19 45 38)) ((42 13 13 4 26 17 42 18 48) (22 42 23 23 46 49 28 23 7)) ((23 39 17 22 38 39 52 34 8 34 39 22 23 38 34) (10 2 22 3 10 14 14 45 28 14 22 46 43 32 14)) ((20 20 7 36 20 44 33 36) (43 3 20 46 52 20 52 46)) ((29 18 52 19 36 19 42 42 52 19 33 26) (23 23 19 38 23 38 13 16 23 39 30 23))))
             (parse-rucksack-file "test-input"))))

(describe "part 1 utilities"
  (it "should find the common item in a rucksack"
    (should= 1 (common-item [[1 2] [4 1]])))

  (it "should find all common items in test input"
    (should= [16 38 42 22 20 19]
             (common-items (parse-rucksack-file "test-input")))))

(describe "part 1 solutions"
  (it "should solve test data"
    (should= 157 (sum-of-common-items "test-input")))

  (it "should solve input"
    (should= 8109 (sum-of-common-items "input"))))

(describe "Parsing groups"
  (it "parses groups"
    (should= [[[1 2] [3 4] [5 6]] [[7 8] [9 10] [11 12]]]
             (parse-groups ["ab" "cd" "ef" "gh" "ij" "kl"]))))

(describe "Part 2 utilities"
  (it "finds badge in group"
    (should= 3 (find-badge [[3 1 2] [4 6 3] [12 14 3]])))

  (it "finds badges in test-input"
    (should= [18 52] (find-badges (parse-groups (string/split-lines (slurp "test-input")))))))

(describe "Part 2 solution -- the badges"
  (it "should solve test-data"
    (should= 70 (sum-of-badges "test-input")))

  (it "should solve input"
    (should= 0 (sum-of-badges "input")))

  )
