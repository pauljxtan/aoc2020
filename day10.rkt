#lang racket

(require "utils.rkt")

(provide part1 part2)

(define input (map string->number (file->lines "input/d10")))

(define (part1)
  (let ([diffs (rating-diffs input)])
    (* (count-value 1 diffs) (count-value 3 diffs))))

(define (part2) 'todo)

(define (rating-diffs ratings) 
  ; Prepend a rating of 0 for the charging outlet and append the device rating
  (let* ([sorted (sort ratings <)]
         [full (append '(0) sorted (list (+ (last sorted) 3)))])
    (map - (drop full 1) (drop-right full 1))))
