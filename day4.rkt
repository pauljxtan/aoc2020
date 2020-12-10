#lang racket

(require "utils.rkt")

(provide
  part1
  part2
  valid-byr?
  valid-ecl?
  valid-eyr?
  valid-hcl?
  valid-hgt?
  valid-iyr?
  valid-pid?)

(define input (load-strings "input/d4"))

(define (part1) (count valid1? passports))

(define (part2) (count valid2? passports))

(define passports
  (foldl (lambda (line result)
           (match line
                  ["" (append result '(""))]
                  [_ (list-set result
                               (- (length result) 1)
                               (string-append (last result) " " line))]))
         (list (first input))
         (list-tail input 1)))

(define (valid1? passport)
  (andmap (lambda (field) (string-contains? passport field))
          '("byr:" "iyr:" "eyr:" "hgt:" "hcl:" "ecl:" "pid:")))

(define (valid2? passport)
  (and (valid1? passport)
       (valid-byr? passport)
       (valid-iyr? passport)
       (valid-eyr? passport)
       (valid-hgt? passport)
       (valid-hcl? passport)
       (valid-ecl? passport)
       (valid-pid? passport)))

(define (valid-byr? passport)
  (match (regexp-match #px"byr:(\\d{4})\\b" passport)
         [(list _ n) (number-in-range? n 1920 2002)]
         [#f #f]))

(define (valid-iyr? passport)
  (match (regexp-match #px"iyr:(\\d{4})\\b" passport)
         [(list _ n) (number-in-range? n 2010 2020)]
         [#f #f]))

(define (valid-eyr? passport)
  (match (regexp-match #px"eyr:(\\d{4})\\b" passport)
         [(list _ n) (number-in-range? n 2020 2030)]
         [#f #f]))

(define (valid-hgt? passport)
  (match (regexp-match #px"hgt:(\\d{2,3})(cm|in)\\b" passport)
         [(list _ n "cm") (number-in-range? n 150 193)]
         [(list _ n "in") (number-in-range? n 59 76)]
         [#f #f]))

(define (valid-hcl? passport)
  (regexp-match? #px"hcl:#[0-9a-f]{6}\\b" passport))

(define (valid-ecl? passport)
  (regexp-match? #px"ecl:(amb|blu|brn|gry|grn|hzl|oth)\\b" passport))

(define (valid-pid? passport)
  (regexp-match? #px"pid:\\d{9}\\b" passport))

(define (number-in-range? number-str low high)
  (let ([n (string->number number-str)]) (and (>= n low) (<= n high))))
