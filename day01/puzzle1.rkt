#lang racket
(define (parse filename)
  (append (file->lines filename)))

(define (mapper list-of-strings)
  (if (empty? list-of-strings)
      0
      (+ ((if (string-prefix? (car list-of-strings) "-") - +)
          (string->number (substring (car list-of-strings) 1)))
         (mapper (cdr list-of-strings)))))

(display (mapper (parse "input1.txt")))