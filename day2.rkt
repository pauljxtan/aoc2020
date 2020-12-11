#lang racket

(provide part1 part2)

(define input (file->lines "input/d2"))

(define (part1) (count identity (map valid1? input)))

(define (part2) (count identity (map valid2? input)))

(define (valid1? line)
  (match (parse-line line)
         [(list min-valid max-valid letter password)
          (let* ([regex-not-letter (regexp (string-append "[^" letter "]"))]
                 [count (string-length (string-replace password
                                                       regex-not-letter
                                                       ""))])
            (and (>= count min-valid) (<= count max-valid)))]))

(define (valid2? line)
  (match (parse-line line)
         [(list i j letter password)
          (xor
            (equal? (substring password (- i 1) i) letter)
            (equal? (substring password (- j 1) j) letter))]))

(define (parse-line line)
  (match (regexp-match #px"(\\d+)-(\\d+) (\\w): (\\w+)" line)
         [(list _ min-valid max-valid letter password)
          (list (string->number min-valid)
                (string->number max-valid)
                letter
                password)]))
