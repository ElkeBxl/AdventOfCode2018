#lang racket

(define (parse filename)
  (map char->integer (car (map string->list (file->lines filename)))))

(define (compare char1 char2)
  (or (= (- char1 32) char2)
      (= char1 (- char2 32))))

(define (solve chars previous-chars)
  (if (or (empty? chars) (< (length chars) 2))
      (length (map integer->char (reverse (append chars previous-chars))))
      (if (compare (car chars) (cadr chars))
          (solve (cons (car previous-chars) (cddr chars)) (cdr previous-chars))
          (solve (cdr chars) (cons (car chars) previous-chars)))))
      
(define parsed-file (parse "input1.txt"))
(solve parsed-file (list))