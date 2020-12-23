#lang racket

(require point-free rackunit)

(provide part1 part2 tests)

(define input (file->string "input/d6"))

(define (part1) (apply + (map count-any-yes (make-groups input))))

(define (part2) (apply + (map count-all-yes (make-groups input))))

(define/compose make-groups
                (curry map (curryr string-split "\n"))
                (curryr string-split "\n\n"))

(define/compose count-any-yes
                set-count
                list->set
                string->list
                (curryr string-join ""))

(define (count-all-yes group)
  (match (map person-set group)
         [(list x xs ...) (set-count (foldl set-intersect x xs))]))

(define/compose person-set list->set string->list)

(define-test-suite
  tests
  (test-case "count questions to which anyone answered yes"
             (check-equal? (count-any-yes '("abc")) 3)
             (check-equal? (count-any-yes '("a" "b" "c")) 3)
             (check-equal? (count-any-yes '("ab" "bc")) 3)
             (check-equal? (count-any-yes '("a" "a" "a" "a")) 1)
             (check-equal? (count-any-yes '("b")) 1))
  (test-case "count questions to which everyone answered yes"
             (check-equal? (count-all-yes '("abc")) 3)
             (check-equal? (count-all-yes '("a" "b" "c")) 0)
             (check-equal? (count-all-yes '("ab" "bc")) 1)
             (check-equal? (count-all-yes '("a" "a" "a" "a")) 1)
             (check-equal? (count-all-yes '("b")) 1)))
