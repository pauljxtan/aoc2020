#lang racket

(provide part1 part2 seat-id seat-row seat-col)

(define input (file->lines "input/d5"))

(define (part1) (apply max (map seat-id input)))

(define (part2)
  (let ([seat-ids (map seat-id input)])
    (findf (lambda (id) (not (member id seat-ids)))
           (range (apply min seat-ids) (apply max seat-ids)))))

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
    [else (let ([first (substring str 0 1)]
                [rest (substring str 1)]
                [mid (quotient (+ low high) 2)])
            (cond 
              [(equal? first low-char)
               (seat-helper rest low-char high-char low mid)]
              [(equal? first high-char)
               (seat-helper rest low-char high-char (+ mid 1) high)]))]))
