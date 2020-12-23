#lang racket

(require
  rackunit
  rackunit/text-ui
  (prefix-in day1: "day1.rkt")
  (prefix-in day2: "day2.rkt")
  (prefix-in day3: "day3.rkt")
  (prefix-in day4: "day4.rkt")
  (prefix-in day5: "day5.rkt")
  (prefix-in day6: "day6.rkt")
  (prefix-in day7: "day7.rkt")
  (prefix-in day8: "day8.rkt")
  (prefix-in day9: "day9.rkt")
  (prefix-in day10: "day10.rkt")
  (prefix-in day11: "day11.rkt"))

(define-test-suite 
  solution-tests
  (check-equal? (day1:part1) 436404)
  (check-equal? (day1:part2) 274879808)
  (check-equal? (day2:part1) 600)
  (check-equal? (day2:part2) 245)
  (check-equal? (day3:part1) 195)
  (check-equal? (day3:part2) 3772314000)
  (check-equal? (day4:part1) 226)
  (check-equal? (day4:part2) 160)
  (check-equal? (day5:part1) 922)
  (check-equal? (day5:part2) 747)
  (check-equal? (day6:part1) 6799)
  (check-equal? (day6:part2) 3354)
  (check-equal? (day7:part1) 238)
  (check-equal? (day7:part2) 82930)
  (check-equal? (day8:part1) 1814)
  (check-equal? (day8:part2) 1056)
  (check-equal? (day9:part1) 138879426)
  (check-equal? (day9:part2) 23761694)
  (check-equal? (day10:part1) 1700)
  (check-equal? (day10:part2) 12401793332096)
  (check-equal? (day11:part1) 2164)
  (check-equal? (day11:part2) 1974))

(run-tests solution-tests)
(run-tests day4:tests)
(run-tests day5:tests)
(run-tests day6:tests)
(run-tests day7:tests)
