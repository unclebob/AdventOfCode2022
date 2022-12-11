(ns day11-monkey-in-the-middle.core-spec
  (:require [speclj.core :refer :all]
            [day11-monkey-in-the-middle.core :refer :all]))

(defn div3 [worry] (quot worry 3))

(declare m0-rule m1-rule m2-rule m3-rule
         monkey0 monkey1 monkey2 monkey3
         monkeys)
(describe "how monkeys work"
  (with m0-rule [* :old 19])
  (with m1-rule [+ :old 6])
  (with m2-rule [* :old :old])
  (with m3-rule [+ :old 3])
  (with monkey0 {:rule @m0-rule :decision [23 2 3]
                 :items [79 98] :inspections 0})
  (with monkey1 {:rule @m1-rule :decision [19 2 0]
                 :items [54 65 75 74] :inspections 0})
  (with monkey2 {:rule @m2-rule :decision [13 1 3]
                 :items [79 60 97] :inspections 0})
  (with monkey3 {:rule @m3-rule :decision [17 0 1]
                 :items [74] :inspections 0})
  (with monkeys {0 @monkey0
                 1 @monkey1
                 2 @monkey2
                 3 @monkey3})

  (it "inspects and computes worry"
    (should= 500 (inspect 79 div3 @m0-rule))
    (should= 20 (inspect 54 div3 @m1-rule))
    (should= 2080 (inspect 79 div3 @m2-rule)))

  (it "decides where to throw"
    (should= 3 (decide-where-to-throw (inspect 79 div3 @m0-rule) @monkey0))
    (should= 1 (decide-where-to-throw (inspect 79 div3 @m2-rule) @monkey2))
    (should= 3 (decide-where-to-throw (inspect 60 div3 @m2-rule) @monkey2)))

  (it "throws between monkeys"
    (let [new-monkeys (throw-one-item @monkeys div3 0)
          new-monkey-0 (new-monkeys 0)
          new-monkey-3 (new-monkeys 3)]
      (should= [98] (:items new-monkey-0))
      (should= 1 (:inspections new-monkey-0))
      (should= [74 500] (:items new-monkey-3))))

  (it "executes a round for one monkey"
    (let [new-monkeys (do-one-monkey div3 @monkeys 2)
          monkey1 (new-monkeys 1)
          monkey2 (new-monkeys 2)
          monkey3 (new-monkeys 3)]
      (should= 3 (:inspections monkey2))
      (should= [] (:items monkey2))
      (should= [54 65 75 74 2080] (:items monkey1))
      (should= [74 1200 3136] (:items monkey3))))

  (it "does one round"
    (let [new-monkeys (do-one-round @monkeys div3 )]
      (let [monkey0 (new-monkeys 0)
            monkey1 (new-monkeys 1)
            monkey2 (new-monkeys 2)
            monkey3 (new-monkeys 3)]
        (should= [20 23 27 26] (:items monkey0))
        (should= [2080 25 167 207 401 1046] (:items monkey1))
        (should= [] (:items monkey2))
        (should= [] (:items monkey3)))))

  (context "parse the monkeys"
    (it "should parse a monkey"
      (should= [0 @monkey0] (parse-monkey
                              ["Monkey 0:"
                               "Starting items: 79, 98"
                               "Operation: new = old * 19"
                               "Test: divisible by 23"
                               "If true: throw to monkey 2"
                               "If false: throw to monkey 3"])))
    (it "should parse test input"
      (should= @monkeys (parse-monkeys "test-input")))))

(defn new-worry-reduction [n] (mod n 9699690))
(defn test-worry-reduction [n] (mod n 96577))

(describe "Solutions"
  (context "part 1"
    (it "solves test input"
      (should= 10605 (monkey-business "test-input" div3 20)))

    (it "solves input"
      (should= 57348 (monkey-business "input" div3 20))))

  (context "part 2 with new worry reduction"
    (it "finds mode for test data"
      (should= 96577 (find-mode (parse-monkeys "test-input"))))

    (it "finds mode for input"
      (should= 9699690 (find-mode (parse-monkeys "input"))))

    (it "tries one round"
      (let [monkeys (parse-monkeys "test-input")
            monkeys (do-n-rounds monkeys 1 test-worry-reduction)
            inspections (map :inspections (vals monkeys))]
        (should= [2 4 3 6] inspections)))

    (it "tries 20 rounds"
      (let [monkeys (parse-monkeys "test-input")
            monkeys (do-n-rounds monkeys 20 test-worry-reduction)
            inspections (map :inspections (vals monkeys))]
        (should= [99 97 8 103] inspections)))

    (it "tries 1000 rounds"
          (let [monkeys (parse-monkeys "test-input")
                monkeys (do-n-rounds monkeys 1000 test-worry-reduction)
                inspections (map :inspections (vals monkeys))]
            (should= [5204 4792 199 5192] inspections)))

    (it "solves test-input"
      (should= 2713310158 (monkey-business "test-input" test-worry-reduction 10000)))

    (it "solves input"
      (should= 14106266886 (monkey-business "input" new-worry-reduction 10000)))))

