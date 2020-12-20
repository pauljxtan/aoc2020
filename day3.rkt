#lang racket

(provide part1 part2)

(define input (map string->list (file->lines "input/d3")))

(define (part1) (move 0 0 1 3 0))

(define (part2) (* (move 0 0 1 1 0)
                   (move 0 0 1 3 0)
                   (move 0 0 1 5 0)
                   (move 0 0 1 7 0)
                   (move 0 0 2 1 0)))

(define (move x y dx dy tree-count)
  (if (>= x (length input))
    tree-count
    (move (+ x dx) (+ y dy) dx dy (+ tree-count (if (is-tree? x y) 1 0)))))

(define (is-tree? row col) 
  (let ([get-col (curryr list-ref ((compose (curry remainder col) length first)
                                   input))]
        [get-row (curryr list-ref row)])
    (equal? #\# ((compose get-col get-row) input))))
