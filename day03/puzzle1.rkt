#lang racket

(define id car)
(define x-coord (lambda (x) (string->number (caadr x))))
(define y-coord (lambda (x) (string->number (cadadr x))))
(define width (lambda (x) (string->number (caaddr x))))
(define height (lambda (x) (string->number (car (cdaddr x)))))
(define key (lambda (x y) (string-append (number->string x) "x" (number->string y))))

(define (parse filename)
  (map (lambda (x) (let ([splitted (string-split x " " #:repeat? #t)])
                     (list (substring (car splitted) 1)
                           (string-split (string-trim (caddr splitted) ":") ",")
                           (string-split (cadddr splitted) "x"))))
       (file->lines filename)))

(define (fill-x x y width hashset)
  (if (= width 0)
      hashset
      (begin (if (hash-has-key? hashset (key (+ x width) y))
          (hash-set! hashset (key (+ x width) y) (+ 1 (hash-ref hashset (key (+ x width) y))))
          (hash-set! hashset (key (+ x width) y) 1))
      (fill-x x y (- width 1) hashset))))

(define (fill x y width height hashset)
  (if (= height 0)
      hashset
      (begin (fill-x x (+ y height) width hashset)
      (fill x y width (- height 1) hashset))))

(define (solve lines hashset)
  (if (empty? lines)
      (length (filter (lambda (x) (> (cdr x) 1)) (hash->list hashset)))
      (begin (fill (x-coord (car lines)) (y-coord (car lines)) (width (car lines)) (height (car lines)) hashset)
      (solve (cdr lines) hashset))))

(define parsed-file (parse "input1.txt"))
(display (solve parsed-file (make-hash)))