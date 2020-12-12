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
  (if (>= curr-row (length input))
    tree-count
    (move (+ curr-row rows-to-move)
          (+ curr-col cols-to-move)
          rows-to-move
          cols-to-move
          (+ tree-count (if (is-tree? curr-row curr-col) 1 0)))))

(define (is-tree? row col) 
  (let* ([col-folded ((compose (curry remainder col) length first) input)]
         [point-func (compose (curryr list-ref col-folded)
                              (curryr list-ref row))]
         [point (point-func input)])
    (equal? point #\#)))
