#lang racket

(require
  rackunit
  rackunit/text-ui
  (prefix-in utils: "utils.rkt")
  (prefix-in day1: "day1.rkt")
  (prefix-in day2: "day2.rkt")
  (prefix-in day3: "day3.rkt")
  (prefix-in day4: "day4.rkt")
  (prefix-in day5: "day5.rkt")
  (prefix-in day6: "day6.rkt"))

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
  (check-equal? (day6:part2) 3354))

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
             (check-false (day4:valid-pid? "pid:0123456789"))))

(define-test-suite
  day5-tests
  (test-case "gets correct row"
             (check-equal? (day5:seat-row "FBFBBFF") 44)
             (check-equal? (day5:seat-row "BFFFBBF") 70)
             (check-equal? (day5:seat-row "FFFBBBF") 14)
             (check-equal? (day5:seat-row "BBFFBBF") 102))
  (test-case "gets correct column"
             (check-equal? (day5:seat-col "RLR") 5)
             (check-equal? (day5:seat-col "RRR") 7)
             (check-equal? (day5:seat-col "RRR") 7)
             (check-equal? (day5:seat-col "RLL") 4))
  (test-case "gets correct seat ID"
             (check-equal? (day5:seat-id "FBFBBFFRLR") 357)
             (check-equal? (day5:seat-id "BFFFBBFRRR") 567)
             (check-equal? (day5:seat-id "FFFBBBFRRR") 119)
             (check-equal? (day5:seat-id "BBFFBBFRLL") 820)))

(define-test-suite
  day6-tests
  (test-case "count questions to which anyone answered yes"
             (check-equal? (day6:count-any-yes '("abc")) 3)
             (check-equal? (day6:count-any-yes '("a" "b" "c")) 3)
             (check-equal? (day6:count-any-yes '("ab" "bc")) 3)
             (check-equal? (day6:count-any-yes '("a" "a" "a" "a")) 1)
             (check-equal? (day6:count-any-yes '("b")) 1))
  (test-case "count questions to which everyone answered yes"
             (check-equal? (day6:count-all-yes '("abc")) 3)
             (check-equal? (day6:count-all-yes '("a" "b" "c")) 0)
             (check-equal? (day6:count-all-yes '("ab" "bc")) 1)
             (check-equal? (day6:count-all-yes '("a" "a" "a" "a")) 1)
             (check-equal? (day6:count-all-yes '("b")) 1)))

(run-tests utils-tests 'verbose)
(run-tests solution-tests 'verbose)
(run-tests day4-tests 'verbose)
(run-tests day5-tests 'verbose)
(run-tests day6-tests 'verbose)
