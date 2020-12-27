#lang racket

(require point-free "utils.rkt")

(provide part1 part2)

(define/compose get-input (curry map string->number) file->lines)

(define input (get-input "input/d9"))

(define (part1) (find-not-sum-of-pair (take input 25) (drop input 25)))

(define (part2) (let ([range (find-range input (part1) 0 1)])
                  (+ (apply min range) (apply max range))))

(define/match (find-not-sum-of-pair nums-with-pair nums-to-check)
  [((list x xs ...) (list y ys ...)) 
   (if (sum-of-pair? y nums-with-pair)
     (find-not-sum-of-pair (append xs (list y)) ys) y)])

(define/match (sum-of-pair? num nums)
  [(_ '()) #f]
  [(_ (list x xs ...)) (or (list-member? (- num x) nums)
                           (sum-of-pair? num xs))])

(define (find-range nums sum start end)
  (let* ([xs (list-slice nums start end)]
         [y (apply + xs)])
    (cond
      [(= y sum) xs]
      [(> y sum) (find-range nums sum (+ start 1) (+ start 2))]
      [else (find-range nums sum start (+ end 1))])))
