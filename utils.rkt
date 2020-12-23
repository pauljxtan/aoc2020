#lang racket

(provide
  count-value
  list-member?
  list-slice
  string-take
  string-drop
  string-first
  string-rest)

; TODO curry
(define (count-value value lst) (count (curry equal? value) lst))

; TODO curry
(define (list-member? elem lst) (not (equal? (member elem lst) #f)))

(define (list-slice lst start end) (take (drop lst start) (- end start)))

; TODO curry
(define (string-take n str) (substring str 0 n))

; TODO curry
(define (string-drop n str) (substring str n))

(define string-first (curry string-take 1))

(define string-rest (curry string-drop 1))

