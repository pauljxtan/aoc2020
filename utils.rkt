#lang racket

(provide list-member? list-slice)

(define (list-member? x xs) (not (equal? (member x xs) #f)))

(define (list-slice xs i j) (take (drop xs i) (- j i)))
