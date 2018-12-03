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

(define (find-id id 1-squares)
  (andmap (lambda (x) (= x 1))
          (for*/list ([new-x (in-range (+ (x-coord id) 1) (+ (x-coord id) (width id) 1))]
                      [new-y (in-range (+ (y-coord id) 1) (+ (y-coord id) (height id) 1))])
              (hash-ref 1-squares (key new-x new-y)))))

(define (find-ids ids 1-squares)
  (if (empty? ids)
      #f
      (if (find-id (car ids) 1-squares)
          (car ids)
          (find-ids (cdr ids) 1-squares))))

(define (solve lines hashset original-lines)
  (if (empty? lines)
      (find-ids original-lines hashset)
      (begin (fill (x-coord (car lines)) (y-coord (car lines)) (width (car lines)) (height (car lines)) hashset)
      (solve (cdr lines) hashset original-lines))))

(define parsed-file (parse "input1.txt"))
(display (solve parsed-file (make-hash) parsed-file))