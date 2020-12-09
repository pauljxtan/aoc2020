#lang racket

(provide load-ints load-strings)

(define (load-ints path) (map string->number (load-strings path)))

(define (load-strings path) (file->lines path))
