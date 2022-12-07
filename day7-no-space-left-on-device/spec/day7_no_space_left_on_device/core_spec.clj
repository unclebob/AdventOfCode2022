(ns day7-no-space-left-on-device.core-spec
  (:require [speclj.core :refer :all]
            [day7-no-space-left-on-device.core :refer :all]))

(describe "building directories"
  (it "moves to /"
    (should= [["/"] :directory] (exec-command :path :directory "$ cd /")))

  (it "moves down a directory"
    (should= [["/" "d"] :directory] (exec-command ["/"] :directory "$ cd d")))

  (it "moves up a directory"
    (should= [["/"] :directory] (exec-command ["/" "d"] :directory "$ cd ..")))

  (it "adds a directory"
    (should= [["/"] {"/" {"a" {}}}]
             (exec-command ["/"] {"/" {}} "dir a")))

  (it "adds a file"
    (should= [["/"] {"/" {"f" 99}}]
             (exec-command ["/"] {"/" {}} "99 f")))

  (it "ignores ls"
    (should= [:path :directory] (exec-command :path :directory "$ ls")))

  (it "builds the test directory"
    (should= {"/" {"a" {"e" {"i" 584}, "f" 29116, "g" 2557, "h.lst" 62596}, "b.txt" 14848514, "c.dat" 8504156, "d" {"j" 4060174, "d.log" 8033020, "d.ext" 5626152, "k" 7214296}}}
             (exec-command-file "test-input")))
  )

(describe "directory size calculation"
  (it "calculates degenerate directory"
    (should= 0 (dir-size {})))

  (it "calculates a directory with only files"
    (should= 100 (dir-size {"f" 99 "g" 1})))

  (it "calculates a directory with files and directories"
    (should= 110 (dir-size {"f" 99 "g" 1 "d" {"k" 10}})))

  (it "maps a simple directory"
    (should= {["/"] 99} (map-directories [] {"/" {"f" 99}})))

  (it "maps a hierarchy"
    (should= {["/"] 99
              ["/" "d"] 88}
             (map-directories [] {"/" {"f" 11
                                    "d" {"h" 88}}})))

  (it "maps a deep hierarchy"
      (should= {["/"] 108
                ["/" "d"] 97
                ["/" "d" "dd"] 9}
               (map-directories [] {"/" {"f" 11
                                      "d" {"h" 88
                                           "dd" {"k" 9}}}})))
  )

(describe "Part 1"
  (it "solves test input"
    (should= 95437 (total-small-directories "test-input")))

  (it "solves input"
    (should= 1611443 (total-small-directories "input"))))

(describe "Part 2 - find best directory to delete"
  (it "solves test-input"
    (should= 24933642 (find-best-to-delete "test-input")))

  (it "solves input"
    (should= 2086088 (find-best-to-delete "input")))
  )

