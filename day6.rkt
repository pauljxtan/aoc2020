#lang racket

(require "utils.rkt")

(provide part1 part2 count-any-yes count-all-yes)

(define input (load-strings "input/d6"))

(define (part1) (foldl + 0 (map count-any-yes groups)))

(define (part2) (foldl + 0 (map count-all-yes groups)))

(define groups
  (foldl (lambda (line result)
           (match line
                  ["" (append result (list (list)))]
                  [_ (list-set result
                               (- (length result) 1)
                               (append (last result) (list line)))]))
         (list (list (first input)))
         (list-tail input 1)))

(define (count-any-yes group)
  (length (remove-duplicates (string->list (string-join group "")))))

(define (count-all-yes group)
  (let ([person-sets (map (lambda (p) (list->set (string->list p))) group)])
    (set-count (foldl set-intersect (first person-sets) person-sets))))

