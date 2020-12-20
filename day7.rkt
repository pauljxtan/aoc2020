#lang racket

(require "utils.rkt")

(provide part1 part2 parse-line)

(define input (file->lines "input/d7"))

(define (part1)
  (count identity (map (compose (curry has-descendant? "shiny gold")
                                first
                                parse-line)
                       input)))

(define (part2) (count-descendants "shiny gold"))

(define (parse-line line)
  (match (regexp-match #px"(\\w+ \\w+) bags contain (.*)" line)
         [(list _ parent "no other bags.") (list parent '())]
         [(list _ parent rest)
          (list parent (map (match-lambda [(list n c)
                                           (cons (string->number n) c)])
                            (regexp-match* #px"(\\d+) (\\w+ \\w+) bags*"
                                           rest
                                           #:match-select cdr)))]))

(define bag-hash
    (foldl (match-lambda** [((list k v) h) (hash-set h k v)])
           (hash)
           (map parse-line input)))

(define (has-descendant? bag-to-find bag) 
  (let ([children (map cdr (hash-ref bag-hash bag))])
    (or (list-member? bag-to-find children)
        (ormap (curry has-descendant? bag-to-find) children))))

(define (count-descendants bag)
  (apply + (map (match-lambda [(cons n c) (* n (+ 1 (count-descendants c)))])
                (hash-ref bag-hash bag))))
