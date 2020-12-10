#lang racket

(require data/maybe "utils.rkt")

(provide part1 part2)

(define input (load-ints "input/d1"))

(define (part1) (foldl * 1 (from-just! (find-pair input 2020))))

(define (part2) (foldl * 1 (from-just! (find-triplet input 2020))))

(define (find-pair entries target-sum)
  (match entries
         ['() nothing]
         [_ (let* ([x (first entries)] 
                   [xs (rest entries)] 
                   [y (- target-sum x)]) 
              (cond 
                [(member y xs) (just (list x y))] 
                [else (find-pair xs target-sum)]))]))

(define (find-triplet entries target-sum)
  (match entries
         ['() nothing]
         [_ (let* ([x (first entries)]
                   [xs (rest entries)]) 
              (match (find-pair xs (- target-sum x)) 
                     [(just (list y z)) (just (list x y z))] 
                     [nothing (find-triplet xs target-sum)]))]))
