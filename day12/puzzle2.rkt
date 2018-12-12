#lang racket

(define make-data cons)
(define get-initial-state car)
(define get-evolutions cdr)
(define get-evolution-initial car)
(define get-evolution-result cadr)
(define make-evolution cons)

(define (map-evolution-line line)
  (let* ([splitted (string-split line " => ")]
         [initials (car splitted)]
         [result (car (string->list (cadr splitted)))])
    (make-evolution initials result)))

(define (parse filename)
  (let* ([lines (file->lines filename)]
         [line-initial-state (car lines)]
         [evolution-lines (cddr lines)])
    (make-data (string-trim line-initial-state "initial state: ")
               (map map-evolution-line evolution-lines))))

(define (expand s)
  (string-append "...." s "...."))

(define (find-in-string x s)
  (let loop ([index 0])
    (if (= index (string-length s))
        #f
        (if (string-prefix? (substring s index) x)
            index
            (loop (+ index 1))))))

(define (find-match state idx evolutions)
  (if (empty? evolutions)
      '()
      (let ([evolution (car evolutions)]
            [state-in-scope (substring state (max 0 (- idx 2)) (min (+ idx 3) (string-length state)))])
        (if (string-contains? state-in-scope (get-evolution-initial evolution))
            (cons (cons idx evolution) (find-match state idx (cdr evolutions)))
            (find-match state idx (cdr evolutions))))))

(define (apply-match state idx evolution)
  (let ([initial (car evolution)]
        [result (cdr evolution)])
    (string-append (substring state 0 idx) (string result) (substring state (+ idx 1)))))

(define (calculate state generation)
  (for/sum ([i (in-range 0 (string-length state))])
    (if (char=? #\# (string-ref state i))
        (- i (* generation 4))
        0)))

(define (solve data)
  (let ([initial-state (get-initial-state data)]
        [evolutions (get-evolutions data)])
    (let loop ([state (expand initial-state)]
               [previous-state "x"]
               [size (+ 8 (string-length initial-state))]
               [generation 1])
      (if (string-contains? state previous-state)
          (+ (calculate state generation) (* (- (calculate state generation) (calculate previous-state (- generation 1))) (- 50000000001 generation)))
          (let inner-loop ([new-state (string)]
                           [idx 0])
            (if (< idx (string-length state))
                (let ([matches (find-match state idx evolutions)])
                  (if (empty? matches)
                      (set! new-state (string-append new-state "."))
                      (set! new-state (string-append new-state (string (cddar matches)))))
                  (inner-loop new-state (+ idx 1)))
                (loop (expand new-state) state (+ size 4) (+ generation 1))))))))

(solve (parse "input1.txt"))
