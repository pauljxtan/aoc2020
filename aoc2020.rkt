#lang racket

(require
  (prefix-in day1: "day1.rkt")
  (prefix-in day2: "day2.rkt")
  (prefix-in day3: "day3.rkt")
  (prefix-in day4: "day4.rkt")
  (prefix-in day5: "day5.rkt")
  (prefix-in day6: "day6.rkt")
  (prefix-in day7: "day7.rkt"))

(printf (format "1-1: ~a\n" (day1:part1)))
(printf (format "1-2: ~a\n" (day1:part2)))
(printf (format "2-1: ~a\n" (day2:part1)))
(printf (format "2-2: ~a\n" (day2:part2)))
(printf (format "3-1: ~a\n" (day3:part1)))
(printf (format "3-2: ~a\n" (day3:part2)))
(printf (format "4-1: ~a\n" (day4:part1)))
(printf (format "4-2: ~a\n" (day4:part2)))
(printf (format "5-1: ~a\n" (day5:part1)))
(printf (format "5-2: ~a\n" (day5:part2)))
(printf (format "6-1: ~a\n" (day6:part1)))
(printf (format "6-2: ~a\n" (day6:part2)))
(printf (format "7-1: ~a\n" (day7:part1)))
