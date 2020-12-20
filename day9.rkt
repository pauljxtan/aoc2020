#lang racket

(require "utils.rkt")

(provide part1 part2)

(define input (map string->number (file->lines "input/d9")))

; TODO split-at instead of take/drop may be more concise here
(define (part1) (find-not-sum-of-pair (take input 25) (drop input 25)))

(define (part2) (let ([range (find-range input (part1) 0 1)])
                  (+ (apply min range) (apply max range))))

(define (find-not-sum-of-pair nums-with-pair nums-to-check)
  (if (sum-of-pair? (first nums-to-check) nums-with-pair)
    (find-not-sum-of-pair (append (rest nums-with-pair)
                                  (list (first nums-to-check)))
                          (rest nums-to-check))
    (first nums-to-check)))

(define (sum-of-pair? num nums)
  (match nums
         ['() #f]
         [_ (or (list-member? (- num (first nums)) nums)
                (sum-of-pair? num (rest nums)))]))

(define (find-range nums target-sum start end)
  (let* ([curr-range (list-slice nums start end)]
         [curr-sum (apply + curr-range)])
    (cond
      [(= curr-sum target-sum) curr-range]
      [(> curr-sum target-sum) (find-range nums target-sum (+ start 1) (+ start 2))]
      [else (find-range nums target-sum start (+ end 1))])))
