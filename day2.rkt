#lang racket

(provide part1 part2)

(define input (file->lines "input/d2"))

(define (part1) (count identity (map valid1? input)))

(define (part2) (count identity (map valid2? input)))

(define (valid1? line)
  (match (parse-line line)
         [(list x y c s) (let ([n (count-char s c)])
                           (and (>= n x) (<= n y)))]))

(define (count-char str char)
  ((compose string-length (curry replace-not-char str)) char))

(define (replace-not-char str char)
  (string-replace str (regex-not-char char) ""))

(define (regex-not-char char) (regexp (string-append "[^" char "]")))

(define (valid2? line)
  (match (parse-line line)
         [(list i j c s) (xor (equal? (substring s (- i 1) i) c)
                              (equal? (substring s (- j 1) j) c))]))

(define (parse-line line)
  (match (regexp-match #px"(\\d+)-(\\d+) (\\w): (\\w+)" line)
         [(list _ x y c s) (list (string->number x) (string->number y) c s)]))
