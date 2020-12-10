#lang racket

(require
  rackunit
  rackunit/text-ui
  (prefix-in utils: "utils.rkt")
  (prefix-in day1: "day1.rkt")
  (prefix-in day2: "day2.rkt")
  (prefix-in day3: "day3.rkt")
  (prefix-in day4: "day4.rkt"))

(define-test-suite 
  solution-tests
  (check-equal? (day1:part1) 436404)
  (check-equal? (day1:part2) 274879808)
  (check-equal? (day2:part1) 600)
  (check-equal? (day2:part2) 245)
  (check-equal? (day3:part1) 195)
  (check-equal? (day3:part2) 3772314000)
  (check-equal? (day4:part1) 226)
  (check-equal? (day4:part2) 160))

(define-test-suite
  utils-tests
  (check-equal? (take (utils:load-ints "input/d1") 10)
                '(1130 1897 1850 1218 1198 1761 1082 1742 1821 1464))
  (check-equal? (take (utils:load-strings "input/d2") 3)
                '("1-13 r: gqdrspndrpsrjfjx"
                  "5-16 j: jjjjkjjzjjjjjfjzjjj"
                  "14-16 r: rrrnrrrrrcnrgxrr")))

(define-test-suite
  day4-tests
  (test-case "validates byr"
             (check-true (day4:valid-byr? "byr:2002"))
             (check-false (day4:valid-byr? "byr:2003")))
  (test-case "validates hgt"
             (check-true (day4:valid-hgt? "hgt:60in"))
             (check-true (day4:valid-hgt? "hgt:190cm"))
             (check-false (day4:valid-hgt? "hgt:190in"))
             (check-false (day4:valid-hgt? "hgt:190")))
  (test-case "validates hcl"
             (check-true (day4:valid-hcl? "hcl:#123abc"))
             (check-false (day4:valid-hcl? "hcl:#123abz"))
             (check-false (day4:valid-hcl? "hcl:123abc")))
  (test-case "validates ecl"
             (check-true (day4:valid-ecl? "ecl:brn"))
             (check-false (day4:valid-ecl? "ecl:wat")))
  (test-case "validates pid"
             (check-true (day4:valid-pid? "pid:000000001"))
             (check-false (day4:valid-pid? "pid:0123456789")))
  )

(run-tests utils-tests)
(run-tests solution-tests)
(run-tests day4-tests 'verbose)
