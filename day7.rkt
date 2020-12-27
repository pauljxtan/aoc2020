#lang racket

(require point-free rackunit "utils.rkt")

(provide part1 part2 tests)

(define input (file->lines "input/d7"))

; TODO refactor
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
  (let ([xs (map cdr (hash-ref bag-hash bag))])
    (or (list-member? bag-to-find xs)
        (ormap (curry has-descendant? bag-to-find) xs))))

(define/compose count-descendants 
                (curry apply +)
                (curry map (match-lambda [(cons n c)
                                          (* n (+ 1 (count-descendants c)))]))
                (curry hash-ref bag-hash))

(define-test-suite
  tests
  (test-case "parses a line into a bag along with the bags it contains"
             (check-equal?
               (parse-line (string-append "light red bags contain "
                                          "4 shiny bronze bags, "
                                          "2 mirrored gray bags, "
                                          "5 dark violet bags."))
               '("light red" ((4 . "shiny bronze")
                              (2 . "mirrored gray")
                              (5 . "dark violet"))))
             (check-equal?
               (parse-line (string-append "bright white bags contain "
                                          "1 posh gold bag, "
                                          "5 mirrored silver bags."))
               '("bright white" ((1 . "posh gold")
                                 (5 . "mirrored silver"))))
             (check-equal?
               (parse-line (string-append "striped magenta bags contain "
                                          "2 drab olive bags, "
                                          "3 dim chartreuse bags, "
                                          "3 plaid beige bags, "
                                          "5 mirrored crimson bags."))
               '("striped magenta" ((2 . "drab olive")
                                    (3 . "dim chartreuse")
                                    (3 . "plaid beige")
                                    (5 . "mirrored crimson"))))))
