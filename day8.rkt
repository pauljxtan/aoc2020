#lang racket

(provide part1 part2)

(define input (file->lines "input/d8"))

(define (part1) (match (execute1 (map parse-instruction input))
                  [(cons acc #f) acc]))

(define (part2) (execute2 (map parse-instruction input)))

(define (execute1-step acc index visited instructions)
  (if (= index (length instructions))
    ; Normal termination (part 2)
    (cons acc #t)
    (if (set-member? visited index)
      ; Termination of infinite loop (part 1)
      (cons acc #f)
      (match (list-ref instructions index)
        [(cons "acc" arg) (execute1-step (+ acc arg)
                                         (+ index 1)
                                         (set-add visited index)
                                         instructions)]
        [(cons "jmp" arg) (execute1-step acc
                                         (+ index arg)
                                         (set-add visited index)
                                         instructions)]
        [(cons "nop" _) (execute1-step acc
                                       (+ index 1)
                                       (set-add visited index)
                                       instructions)]))))

(define (execute2-step index-to-replace instructions)
  (match (execute1-step 0 0 (set) (replace-at instructions index-to-replace))
    [(cons acc #t) acc]
    [(cons _ #f) (execute2-step (+ index-to-replace 1) instructions)]))

(define execute1 (curry execute1-step 0 0 (set)))

(define execute2 (curry execute2-step 0))

(define (parse-instruction line)
  (match (regexp-match #px"(acc|jmp|nop) ([\\+\\-]?\\d+)" line)
    [(list _ op arg) (cons op (string->number arg))]))

(define (replace-at instructions index)
  (match (list-ref instructions index)
    [(cons "acc" _) instructions]
    [(cons "jmp" arg) (list-set instructions index (cons "nop" arg))]
    [(cons "nop" arg) (list-set instructions index (cons "jmp" arg))]))
