#lang racket
(define (parse filename)
  (map string->list (file->lines filename)))

(define (counter id hashset)
  (if (empty? id)
      (filter (lambda (x) (or (= (cdr x) 2) (= (cdr x) 3))) (hash->list hashset))
      (begin
        (if (hash-has-key? hashset (car id))
            (hash-set! hashset (car id) (+ 1 (hash-ref hashset (car id))))
            (hash-set! hashset (car id) 1))
        (counter (cdr id) hashset))))      

(define (mapper ids)
  (map (lambda (id) (counter id (make-weak-hash))) ids))

(define (solver counts times-2 times-3)
  (if (empty? counts)
      (* times-2 times-3)
      (begin
        (let ([found-2 (count (lambda (x) (= (cdr x) 2)) (car counts))]
              [found-3 (count (lambda (x) (= (cdr x) 3)) (car counts))])
        (if (and (>= found-2 1) (>= found-3 1))
            (solver (cdr counts) (+ times-2 1) (+ times-3 1))
            (if (>= found-2 1)
                (solver (cdr counts) (+ times-2 1) times-3)
                (if (>= found-3 1)
                    (solver (cdr counts) times-2 (+ times-3 1))
                    (solver (cdr counts) times-2 times-3))))))))
      

(define parsed-file (parse "input1.txt"))
(display (solver (mapper parsed-file) 0 0))