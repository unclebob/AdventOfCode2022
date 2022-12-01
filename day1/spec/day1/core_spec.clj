(ns day1.core-spec
  (:require [speclj.core :refer :all]
            [day1.core :refer :all]
            [clojure.string :as string]))

(describe "Parse Input"
  (it "reads file"
    (should= "1000\n2000\n3000\n\n4000\n\n5000\n6000\n\n7000\n8000\n9000\n\n10000"
             (slurp "test-input")))

  (it "breaks input into lines"
    (should= ["this" "that" "" "the other"] (string/split-lines "this\nthat\n\nthe other")))

  (it "Parses input"
    (should= [[1000 2000 3000] [4000] [5000 6000] [7000 8000 9000] [10000]]
             (parse-inventory "test-input"))))

(describe "finding elf with maximum calories"
  (it "should sum up calories"
    (should= [6000 4000 11000 24000 10000]
             (sum-calories [[1000 2000 3000] [4000] [5000 6000] [7000 8000 9000] [10000]])))

  (it "finds maximum calories"
    (should= 24000 (max-calories [1000 24000 19000]))))

(describe "solutions"
  (it "solves part 1 finding the maximum calories"
    (should= 66306 (find-max-calories "input")))

  (it "solves part 2 finding sum of top three elves"
    (should= 195292 (find-calories-in-top-three-elves "input"))))

