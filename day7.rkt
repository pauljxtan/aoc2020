#lang racket

(provide part1 part2 bag-hash)

(define input (file->lines "input/d7"))

(define (part1)
  (let ([bags (map first (map parse-line input))])
    (count identity (map (curry has-descendant? "shiny gold") bags))))

(define (part2) 'todo)

(define (parse-line line)
  (match (regexp-match #px"(\\w+ \\w+) bags contain (.*)" line)
         [(list _ parent "no other bags.") (list parent '())]
         [(list _ parent rest) (list parent
                                     (regexp-match* #px"\\d+ (\\w+ \\w+) bags*"
                                                    rest
                                                    #:match-select cadr))]))

(define bag-hash
  (let ([bags (map parse-line input)])
    (foldl (lambda (b h) (match b [(list k v) (hash-set h k v)]))
           (hash)
           bags)))

(define (has-descendant? bag-to-find bag) 
  (let ([children (hash-ref bag-hash bag)])
    (or (list-member? bag-to-find children)
        (ormap (curry has-descendant? bag-to-find) children))))

(define (list-member? x xs) (not (equal? (member x xs) #f)))
