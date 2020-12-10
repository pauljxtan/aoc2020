#lang racket

(require "utils.rkt")

(provide part1 part2 seat-id seat-row seat-col)

(define input (load-strings "input/d5"))

(define (part1) (argmax identity (map seat-id input)))

(define (part2)
  (let* ([seat-ids (map seat-id input)]
         [min-id (argmin identity seat-ids)]
         [max-id (argmax identity seat-ids)])
    (findf (lambda (id) (not (member id seat-ids))) (range min-id max-id))))

(define (seat-id seat-str)
  (let ([row (seat-row (substring seat-str 0 7))]
        [col (seat-col (substring seat-str 7))])
    (+ (* row 8) col)))

(define (seat-row str) (seat-helper str "F" "B" 0 127))

(define (seat-col str) (seat-helper str "L" "R" 0 7))

(define (seat-helper str low-char high-char low high)
  (cond 
    [(equal? str low-char) low]
    [(equal? str high-char) high]
    [else (cond 
            [(equal? (substring str 0 1) low-char)
             (seat-helper (substring str 1)
                          low-char
                          high-char
                          low
                          (quotient (+ low high) 2))]
            [(equal? (substring str 0 1) high-char)
             (seat-helper (substring str 1)
                          low-char
                          high-char
                          (+ (quotient (+ low high) 2) 1) 
                          high)])]))
