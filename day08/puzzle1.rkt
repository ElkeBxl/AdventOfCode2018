#lang racket

(define (parse filename)
  (let ([line (car (file->lines filename))])
    (map string->number (string-split line " "))))


(define letter 64)
(define (get-new-letter)
  (set! letter (+ letter 1))
  (integer->char letter))

(define get-children cadddr)
(define (get-metadata x) (car (cddddr x)))

(define (get-tree original-input)
  (let ([input (list->vector original-input)]
        [idx 0])
    (let loop ([c# (vector-ref input 0)]
               [m# (vector-ref input 1)])
      (set! idx (+ idx 2))
      (list (get-new-letter)
            c#
            m#
            (for/list ([i (in-range 0 c#)])
              (loop (vector-ref input idx)
                    (vector-ref input (+ idx 1))))
            (for/list ([j (in-range 0 m#)])
              (set! idx (+ idx 1))
              (vector-ref input (- idx 1)))))))

(define (solve tree)
  (if (empty? tree)
      0
      (let ([c (get-children tree)]
            [m (get-metadata tree)])
        (if (empty? c)
            (foldl + 0 m)
            (+ (foldl + 0 (get-metadata tree))
               (for/sum ([i c])
                 (solve i)))))))
        
(define parsed-file (parse "input1.txt"))
(solve (get-tree parsed-file))