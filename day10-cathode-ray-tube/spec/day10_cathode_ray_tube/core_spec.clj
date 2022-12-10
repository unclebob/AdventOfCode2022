(ns day10-cathode-ray-tube.core-spec
  (:require [speclj.core :refer :all]
            [day10-cathode-ray-tube.core :refer :all]))

(describe "instruction execution"
  (it "adds a cycle for a nop"
    (should= {:x 1 :cycles [{:x 1}]} (noop {:x 1 :cycles []}))
    (should= {:x 2 :cycles [{:x 2} {:x 2}]} (noop {:x 2 :cycles [{:x 2}]})))

  (it "adds two cycles for an addx"
    (should= {:x 4 :cycles [{:x 2} {:x 2}]}
             (addx 2 {:x 2 :cycles []})))
  )

(describe "executing a program part 1"
  (it "executes a sequence of lines"
    (should= {:x 5 :cycles [{:x 7} {:x 7} {:x 7}]}
             (execute {:x 7 :cycles []} ["noop" "addx -2"]))))

(describe "part 1 solution, signal strength"
  (it "solves test data"
    (should= 13140 (signal-strength "test-input")))
  (it "solves input"
    (should= 14620 (signal-strength "input"))))

(describe "part 2 solution, rendering the screen"
  (it "renders state"
    (should= [".........###............................"
              ".........###............................"]
             (render (repeat 80 {:x 10}))))

  (it "renders test-input"
    (should
      (print-screen
        (render (execution-of "test-input")))))

  (it "renders input"
    (should
      (print-screen
        (render (execution-of "input"))))))
