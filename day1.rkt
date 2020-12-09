#lang racket

(require "utils.rkt")

(provide part1 part2)

(define input (load-ints "input/d1"))

(define (part1)
  (match (find-pair input 2020)
         [(list x y) (* x y)]
         [_ 'no-solution]))

(define (part2)
  (match (find-triplet input 2020)
         [(list x y z) (* x y z)]
         ['no-triplet 'no-solution]))

(define (find-pair entries target-sum)
  (match entries
         ['() 'no-pair]
         [_ (let* ([x (first entries)] 
                   [xs (rest entries)] 
                   [y (- target-sum x)]) 
              (cond 
                [(member y xs) (list x y)] 
                [else (find-pair xs target-sum)]))]))

(define (find-triplet entries target-sum)
  (match entries
         ['() 'no-triplet]
         [_ (let* ([x (first entries)]
                   [xs (rest entries)]) 
              (match (find-pair xs (- target-sum x)) 
                     [(list y z) (list x y z)] 
                     ['no-pair (find-triplet xs target-sum)]))]))
