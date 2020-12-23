#lang racket

(require point-free "utils.rkt")

(provide part1 part2)

(define/compose get-input (curry map string->list) file->lines)

(define input (get-input "input/d11"))

(define num-rows (length input))
(define num-cols (length (first input)))

; TODO optimize

(define (part1)
  (let ([final-grid (step1-until-unchanged input)])
    (apply + ((count-occupied-over-grid) final-grid))))

(define (part2)
  (let ([final-grid (step2-until-unchanged input)])
    (apply + (( count-value-over-grid #\# ) final-grid))))

(define (count-occupied-over-grid) (count-value-over-grid #\#))

(define (count-value-over-grid value) (curry map (curry count-value value)))

(define (step1-until-unchanged grid)
  (let ([next-grid (step1 grid)])
    (if (equal? next-grid grid) next-grid (step1-until-unchanged next-grid))))

(define (step1 grid)
  (map-grid-cells
    grid
    (lambda (row col) (let ([seat (grid-ref grid row col)]
                            [adj-occ (count-adjacent-occupied grid row col)])
                        (match seat
                               [#\L (if (= adj-occ 0) #\# seat)]
                               [#\# (if (>= adj-occ 4) #\L seat)]
                               [_ seat]) ))))

(define (step2-until-unchanged grid)
  (let ([next-grid (step2 grid)])
    (if (equal? next-grid grid) next-grid (step2-until-unchanged next-grid))))

(define (step2 grid)
  (map-grid-cells
    grid
    (lambda (row col) (let ([seat (grid-ref grid row col)]
                            [vis-occ (count-visible-occupied grid row col)])
                        (match seat
                               [#\L (if (= vis-occ 0) #\# seat)]
                               [#\# (if (>= vis-occ 5) #\L seat)]
                               [_ seat]) ))))

(define (map-grid-cells grid func)
  (map (lambda (row)
         (map (lambda (col) (func row col))
              (range 0 num-cols)))
       (range 0 num-rows)))

(define directions '(top-left
                      top
                      top-right
                      left
                      right
                      bottom-left
                      bottom
                      bottom-right))

(define (count-adjacent-occupied grid row col)
  (count-value #\# (adjacent-seats grid row col)))


(define (adjacent-seats grid row col)
  (map (lambda (direction) (grid-ref grid
                                     (get-row-to-check row direction) 
                                     (get-col-to-check col direction)))
       directions))

(define (count-visible-occupied grid row col)
  (count-value #t (map (curry visible-occupied? grid row col) directions)))

(define (visible-occupied? grid row col direction)
  (let* ([row-to-check (get-row-to-check row direction)]
         [col-to-check (get-col-to-check col direction)])
    (if (outside-grid? grid row-to-check col-to-check)
      #f
      (match (grid-ref grid row-to-check col-to-check)
             [#\# #t]
             [#\L #f]
             [_ (visible-occupied? grid row-to-check col-to-check direction)]))))

(define (get-row-to-check row direction)
  (+ row (match direction
                [(or 'top-left 'top 'top-right) -1]
                [(or 'left 'right) 0]
                [(or 'bottom-left 'bottom 'bottom-right) +1])))

(define (get-col-to-check col direction)
  (+ col (match direction
                [(or 'top-left 'left 'bottom-left) -1]
                [(or 'top 'bottom) 0]
                [(or 'top-right 'right 'bottom-right) +1])))

; Treat positions outside the grid as floor
(define (grid-ref grid row col)
  (if (outside-grid? grid row col) #\. (list-ref (list-ref grid row) col)))

(define (outside-grid? grid row col)
  (or (< row 0) (>= row num-rows) (< col 0) (>= col num-cols)))
