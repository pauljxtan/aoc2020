#lang racket

(require
  rackunit
  rackunit/text-ui
  (prefix-in utils: "utils.rkt")
  (prefix-in day1: "day1.rkt")
  (prefix-in day2: "day2.rkt")
  (prefix-in day3: "day3.rkt"))

(define utils-tests
  (test-suite "Tests for utils"
              (check-equal?
                (take (utils:load-ints "input/d1") 10)
                '(1130 1897 1850 1218 1198 1761 1082 1742 1821 1464))
              (check-equal?
                (take (utils:load-strings "input/d2") 3)
                '("1-13 r: gqdrspndrpsrjfjx"
                  "5-16 j: jjjjkjjzjjjjjfjzjjj"
                  "14-16 r: rrrnrrrrrcnrgxrr"))))

(define day1-tests
  (test-suite "Tests for Day 1"
              (check-equal? (day1:part1) 436404)
              (check-equal? (day1:part2) 274879808)))

(define day2-tests
  (test-suite "Tests for Day 2"
              (check-equal? (day2:part1) 600)
              (check-equal? (day2:part2) 245)))

(define day3-tests
  (test-suite "Tests for Day 3"
              (check-equal? (day3:part1) 195)
              (check-equal? (day3:part2) 3772314000)))

(run-tests utils-tests)
(run-tests day1-tests)
(run-tests day2-tests)
(run-tests day3-tests)
