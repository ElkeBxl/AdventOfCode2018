#lang racket

(define (parse filename)
  (map char->integer (car (map string->list (file->lines filename)))))

(define (compare char1 char2)
  (or (= (- char1 32) char2)
      (= char1 (- char2 32))))

(define (more-compare char1 char2)
  (or (compare char1 char2)
      (= char1 char2)))

(define (reduce chars previous-chars)
  (if (or (empty? chars) (< (length chars) 2))
      (length (map integer->char (reverse (append chars previous-chars))))
      (if (compare (car chars) (cadr chars))
          (reduce (if (empty? previous-chars) (cddr chars) (cons (car previous-chars) (cddr chars))) (if (empty? previous-chars) previous-chars (cdr previous-chars)))
          (reduce (cdr chars) (cons (car chars) previous-chars)))))

(define (find-smallest l)
  (let loop ([l l]
             [smallest (car l)])
    (cond
      [(empty? l) smallest]
      [(< smallest (car l))
       (loop (cdr l) smallest)]
      [else
       (loop (cdr l) (car l))]))) 

(define (solve chars)
  (let ([resulting-lengths (make-vector 26)])
    (for ([alphabet (in-range 97 123)])
      (let ([new-chars (filter (lambda (x) (not (more-compare x alphabet))) chars)])
        (vector-set! resulting-lengths (- alphabet 97) (reduce new-chars (list)))))
    (find-smallest (vector->list resulting-lengths))))
      
(define parsed-file (parse "input1.txt"))
(solve parsed-file)