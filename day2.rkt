#lang racket

(require
  data/applicative
  data/monad
  megaparsack
  megaparsack/text
  "utils.rkt")

(provide part1 part2)

(define input (load-strings "input/d2"))

(define (part1) (count identity (map valid1? input)))

(define (part2) (count identity (map valid2? input)))

(define (valid1? line)
  (match (parse-line line)
         [(list min-valid max-valid letter password)
          (let ([count (count (lambda (c) (char=? c letter)) password)])
            (and (>= count min-valid) (<= count max-valid)))]))

(define (valid2? line)
  (match (parse-line line)
         [(list i j letter password)
          (xor
            (char=? (list-ref password (- i 1)) letter)
            (char=? (list-ref password (- j 1)) letter))]))

(define (parse-line line) (parse-result! (parse-string line/p line)))

(define line/p
  (do
    [m <- integer/p]
    (string/p "-")
    [n <- integer/p]
    (string/p " ")
    [letter <- letter/p]
    (string/p ": ")
    [password <- (many/p letter/p)]
    (pure (list m n letter password))))
