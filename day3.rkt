#lang racket

(provide part1 part2)

(define input (map string->list (file->lines "input/d3")))

(define (part1) (move 0 0 1 3 0))

(define (part2) (* (move 0 0 1 1 0)
                   (move 0 0 1 3 0)
                   (move 0 0 1 5 0)
                   (move 0 0 1 7 0)
                   (move 0 0 2 1 0)))

(define (move curr-row curr-col rows-to-move cols-to-move tree-count)
  (let* ([the-map input]
         [num-rows (length the-map)])
    (if (>= curr-row num-rows)
      tree-count
      (move (+ curr-row rows-to-move)
            (+ curr-col cols-to-move)
            rows-to-move
            cols-to-move
            (+ tree-count (if (is-tree? the-map curr-row curr-col) 1 0))))))

(define (is-tree? the-map row col) 
  (let ([col-folded (remainder col (length (first the-map)))])
    (equal? (list-ref (list-ref the-map row) col-folded) #\#)))
