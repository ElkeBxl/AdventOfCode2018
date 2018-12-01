#lang racket
(define (parse filename)
  (append (file->lines filename)))

(define (mapper list-of-strings previous-frequencies initial-list)
  (if (empty? list-of-strings)
      (mapper initial-list previous-frequencies initial-list)
      (if (member (car previous-frequencies) (cdr previous-frequencies))
          (car previous-frequencies)
          (mapper (cdr list-of-strings)
                  (cons
                   (+ ((if (string-prefix? (car list-of-strings) "-") - +) (string->number (substring (car list-of-strings) 1)))
                            (car previous-frequencies))
                   previous-frequencies)
                  initial-list))))

(define parsed-file (parse "input1.txt"))
(display (mapper parsed-file (list 0) parsed-file))