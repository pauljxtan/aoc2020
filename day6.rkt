#lang racket

(provide part1 part2 count-any-yes count-all-yes)

(define input (file->string "input/d6"))

(define (part1) (foldl + 0 (map count-any-yes groups)))

(define (part2) (foldl + 0 (map count-all-yes groups)))

(define groups (map (curryr string-split "\n") (string-split input "\n\n")))

(define (count-any-yes group)
  ((compose set-count list->set string->list) (string-join group "")))

(define (count-all-yes group)
  (let ([person-sets (map (compose list->set string->list) group)])
    (set-count (foldl set-intersect (first person-sets) (rest person-sets)))))
