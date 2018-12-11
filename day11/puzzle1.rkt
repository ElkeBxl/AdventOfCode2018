#lang racket

(define (make-coord x y)
  (mcons x y))
(define (x c)
  (mcar c))
(define (y c)
  (mcdr c))
(define (get-key c)
  (string-join (list (number->string (x c)) (number->string (y c))) "x"))

(define serial-number 9306)
(define (get-rack-id c)
  (+ (x c) 10))
(define (get-power-level c)
  (let* ([temp (* (+ (* (y c) (get-rack-id c)) serial-number) (get-rack-id c))]
         [temp-s (number->string temp)]
         [hundreds (if (> (string-length temp-s) 2)
                       (string->number (make-string 1 (string-ref temp-s (- (string-length temp-s) 3))))
                       0)])
    (- hundreds 5)))

(define (calculate-power grid c size)
  (for*/sum ([i (in-range 0 size)]
             [j (in-range 0 size)])
    (hash-ref grid (get-key (make-coord (+ (x c) i) (+ (y c) j))))))

(define (solve serial-number)
  (let ([grid (make-hash)]
        [highest-power 0]
        [highest-coord (make-coord 0 0)]
        [highest-size 1])
    (for ([size (in-range 1 20)])
      (for* ([i (in-range 0 300)]
             [j (in-range 0 300)])
        (hash-set! grid (get-key (make-coord i j)) (get-power-level (make-coord i j))))
      (for* ([i (in-range 0 (- 301 size))]
             [j (in-range 0 (- 301 size))])
        (let* ([c (make-coord i j)]
               [possible-highest-power (calculate-power grid c size)])
          (if (> possible-highest-power highest-power)
              (begin
                (set! highest-power possible-highest-power)
                (set! highest-coord c)
                (set! highest-size size))
              #t))))
    (list highest-power highest-coord highest-size)))             

(solve serial-number)