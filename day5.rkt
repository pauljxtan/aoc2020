#lang racket

(require rackunit "utils.rkt")

(provide part1 part2 tests)

(define input (file->lines "input/d5"))

(define (part1) (apply max (map seat-id input)))

(define (part2)
  (let ([xs (map seat-id input)])
    (findf (lambda (id) (not (member id xs)))
           (range (apply min xs) (apply max xs)))))

(define (seat-id seat-str)
  (let ([x (seat-row (string-take 7 seat-str))]
        [y (seat-col (string-drop 7 seat-str))])
    (+ (* x 8) y)))

(define (seat-index low-char high-char low high str)
  (cond 
    [(equal? str low-char) low]
    [(equal? str high-char) high]
    [else (let ([c (string-first str)]
                [s (string-rest str)]
                [m (quotient (+ low high) 2)])
            (cond 
              [(equal? c low-char)
               (seat-index low-char high-char low m s)]
              [(equal? c high-char)
               (seat-index low-char high-char (+ m 1) high s)]))]))

(define seat-row (curry seat-index "F" "B" 0 127))

(define seat-col (curry seat-index "L" "R" 0 7))

(define-test-suite
  tests
  (test-case "gets correct row"
             (check-equal? (seat-row "FBFBBFF") 44)
             (check-equal? (seat-row "BFFFBBF") 70)
             (check-equal? (seat-row "FFFBBBF") 14)
             (check-equal? (seat-row "BBFFBBF") 102))
  (test-case "gets correct column"
             (check-equal? (seat-col "RLR") 5)
             (check-equal? (seat-col "RRR") 7)
             (check-equal? (seat-col "RRR") 7)
             (check-equal? (seat-col "RLL") 4))
  (test-case "gets correct seat ID"
             (check-equal? (seat-id "FBFBBFFRLR") 357)
             (check-equal? (seat-id "BFFFBBFRRR") 567)
             (check-equal? (seat-id "FFFBBBFRRR") 119)
             (check-equal? (seat-id "BBFFBBFRLL") 820)))
