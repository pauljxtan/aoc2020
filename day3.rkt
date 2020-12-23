#lang racket

(provide part1 part2)

(define input (map string->list (file->lines "input/d3")))

(define (part1) (traverse 1 3))

(define (part2) (apply * (map traverse '(1 1 1 1 2) '(1 3 5 7 1))))

(define (traverse dx dy) (step 0 0 dx dy 0))

(define (step x y dx dy tree-count)
  (if (>= x (length input))
    tree-count
    (step (+ x dx) (+ y dy) dx dy (+ tree-count (if (is-tree? x y) 1 0)))))

(define (is-tree? row col) 
  (let* ([f (compose (curry remainder col) length first)]
         [g (curryr list-ref (f input))]
         [h (curryr list-ref row)])
    (equal? #\# ((compose g h) input))))
