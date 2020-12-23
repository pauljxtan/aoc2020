#lang racket

(require memoize point-free "utils.rkt")

(provide part1 part2)

(define/compose get-input (curryr sort <) (curry map string->number) file->lines)

(define (part1)
  (let ([diffs (rating-diffs (get-input "input/d10"))])
    (* (count-value 1 diffs) (count-value 3 diffs))))

(define (part2)
  (let ([ratings (all-ratings (get-input "input/d10"))])
    (count-paths ratings (last ratings))))

(define (rating-diffs adapters) 
  (let ([ratings (all-ratings adapters)])
    (map - (drop ratings 1) (drop-right ratings 1))))

(define (all-ratings adapters)
  (append '(0) adapters (list (+ (last adapters) 3))))

; Intuition:
; - Each adapter can be preceded by an adapter of either 1, 2, or 3 fewer
;   jolts, so we can recursively sum up the paths leading to each predecessor
; - If an adapter is not present in the list, it effectively has zero paths
;   leading to it
(define/memo
  (count-paths ratings rating)
  (if (equal? rating 0)
    1
    (if (not (member rating ratings))
      0
      (apply + (map (lambda (diff)
                      (let ([prev (- rating diff)])
                        (if (>= prev 0) (count-paths ratings prev) 0)))
                    '(1 2 3))))))
