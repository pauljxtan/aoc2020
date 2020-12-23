#lang racket

(require point-free rackunit)

(provide part1 part2 tests)

(define input (file->string "input/d4"))

(define (part1) (count valid1? (make-passports input)))

(define (part2) (count valid2? (make-passports input)))

(define/compose make-passports
                (curry map (curryr string-replace " " "\n"))
                (curryr string-split "\n\n"))

(define (valid1? passport)
  (andmap (curry string-contains? passport)
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

(define (field-in-range? pattern low high passport)
  (match (regexp-match pattern passport)
         [(list _ n) (number-in-range? n low high)]
         [#f #f]))

(define valid-byr? (curry field-in-range? #px"byr:(\\d{4})\\b" 1920 2002))

(define valid-iyr? (curry field-in-range? #px"iyr:(\\d{4})\\b" 2010 2020))

(define valid-eyr? (curry field-in-range? #px"eyr:(\\d{4})\\b" 2020 2030))

(define (valid-hgt? passport) (or (valid-hgt-cm? passport)
                                  (valid-hgt-in? passport)))
(define valid-hgt-cm? (curry field-in-range? #px"hgt:(\\d{2,3})cm\\b" 150 193))
(define valid-hgt-in? (curry field-in-range? #px"hgt:(\\d{2,3})in\\b" 59 76))

(define valid-hcl? (curry regexp-match? #px"hcl:#[0-9a-f]{6}\\b"))

(define valid-ecl? (curry regexp-match?
                          #px"ecl:(amb|blu|brn|gry|grn|hzl|oth)\\b"))

(define (valid-pid? passport)
  (regexp-match? #px"pid:\\d{9}\\b" passport))

(define (number-in-range? num-str low high)
  (let ([n (string->number num-str)]) (and (>= n low) (<= n high))))

(define-test-suite
  tests
  (test-case "validates byr"
             (check-true (valid-byr? "byr:2002"))
             (check-false (valid-byr? "byr:2003")))
  (test-case "validates hgt"
             (check-true (valid-hgt? "hgt:60in"))
             (check-true (valid-hgt? "hgt:190cm"))
             (check-false (valid-hgt? "hgt:190in"))
             (check-false (valid-hgt? "hgt:190")))
  (test-case "validates hcl"
             (check-true (valid-hcl? "hcl:#123abc"))
             (check-false (valid-hcl? "hcl:#123abz"))
             (check-false (valid-hcl? "hcl:123abc")))
  (test-case "validates ecl"
             (check-true (valid-ecl? "ecl:brn"))
             (check-false (valid-ecl? "ecl:wat")))
  (test-case "validates pid"
             (check-true (valid-pid? "pid:000000001"))
             (check-false (valid-pid? "pid:0123456789"))))
