#lang racket

(provide list-member? list-slice count-value)

(define (list-member? elem lst) (not (equal? (member elem lst) #f)))

(define (list-slice lst start end) (take (drop lst start) (- end start)))

(define (count-value value lst) (count (curry equal? value) lst))
