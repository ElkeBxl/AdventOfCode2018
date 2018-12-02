#lang racket
(define (parse filename)
  (map string->list (file->lines filename)))

(define (compare id1 id2 errors)
  (if (empty? id1)
      #t
      (if (< errors 0)
          #f
          (if (eq? (car id1) (car id2))
              (compare (cdr id1) (cdr id2) errors)
              (compare (cdr id1) (cdr id2) (- errors 1))))))

(define (solver ids)
  (if (empty? ids)
      '()
      (if (findf (lambda (x) (compare (car ids) x 1)) (cdr ids))
          (cons (car ids) (findf (lambda (x) (compare (car ids) x 1)) (cdr ids)))
          (solver (cdr ids)))))

(define (commons list1 list2)
  (if (empty? list1)
      '()
      (if (eq? (car list1) (car list2))
          (cons (car list1) (commons (cdr list1) (cdr list2)))
          (commons (cdr list1) (cdr list2)))))

(define parsed-file (parse "input1.txt"))
(define results (solver parsed-file))
(display (commons (car results) (cdr results)))