#lang racket

(require data/maybe)

(provide part1 part2)

(define input (map string->number (file->lines "input/d1")))

(define (part1) (foldl * 1 (from-just! (find-pair input 2020))))

(define (part2) (foldl * 1 (from-just! (find-triplet input 2020))))

(define (find-pair entries sum)
  (match entries
         ['() nothing]
         [(list x xs ...) (let ([y (- sum x)]) (if (member y xs)
                                                 (just (list x y))
                                                 (find-pair xs sum)))]))

(define (find-triplet entries sum)
  (match entries
         ['() nothing]
         [(list x xs ...) (match (find-pair xs (- sum x)) 
                                 [(just (list y z)) (just (list x y z))] 
                                 [nothing (find-triplet xs sum)])]))
