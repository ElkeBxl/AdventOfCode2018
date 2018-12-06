#lang racket

(define coord cons)
(define x car)
(define y cdr)

(define (parse filename)
  (let ([list (file->lines filename)])
    (map (lambda (a) (coord (string->number (car (string-split a ", "))) (string->number (cadr (string-split a ", "))))) list)))

(define (calc-manhattan-distance c1 c2)
  (+ (- (max (x c1) (x c2)) (min (x c1) (x c2)))
     (- (max (y c1) (y c2)) (min (y c1) (y c2)))))

(define (find-greatest-or-lowest op x-or-y coords greatest-or-lowest-coord)
  (let loop ([greatest greatest-or-lowest-coord]
             [coords coords])
    (cond [(empty? coords) (x-or-y greatest)]
          [(op (x-or-y (car coords)) (x-or-y greatest)) (loop (car coords) (cdr coords))]
          [else (loop greatest (cdr coords))])))

(define (find-greatest x-or-y coords)
  (find-greatest-or-lowest > x-or-y coords (coord 0 0)))

(define (find-lowest x-or-y coords)
  (find-greatest-or-lowest < x-or-y coords (coord 9999999 9999999)))

(define (solve original-coords)
  (let ([greatest-x (find-greatest x original-coords)]
        [greatest-y (find-greatest y original-coords)]
        [lowest-x (find-lowest x original-coords)]
        [lowest-y (find-lowest y original-coords)]
        [greatest-size 0])
    (for* ([idx-x (in-range lowest-x (+ greatest-x 1))]
           [idx-y (in-range lowest-y (+ greatest-y 1))])
      (let ([new-coord (coord idx-x idx-y)])
        (let loop ([coords original-coords]
                   [current-sum 0])
          (cond [(and (empty? coords) (< current-sum 10000)) (set! greatest-size (+ 1 greatest-size))]
                [(empty? coords) #t]
                [else (loop (cdr coords) (+ current-sum (calc-manhattan-distance new-coord (car coords))))]))))
    greatest-size))

(solve (parse "input1.txt"))