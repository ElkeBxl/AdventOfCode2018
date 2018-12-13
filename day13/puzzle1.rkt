#lang racket

(define make-grid make-hash)
(define (make-key x y)
  (string-append (number->string x) "x" (number->string y)))
(define (coord-get g x y)
  (hash-ref g (make-key x y)))
(define (coord-exists? g x y)
  (hash-has-key? g (make-key x y)))
(define (coord-set! g x y v)
  (hash-set! g (make-key x y) v))

(define make-carts list)
(define (add-cart! c carts)
  (cons c carts))
(define (make-position x y)
  (mcons x y))
(define (make-cart x y direction next-turns)
  (mcons (make-position x y) (mcons direction next-turns)))
(define get-position mcar)
(define set-position! set-mcar!)
(define (get-position-x c)
  (mcar (mcar c)))
(define (get-position-y c)
  (mcdr (mcar c)))
(define (get-direction c)
  (mcar (mcdr c)))
(define (set-direction! c v)
  (set-mcar! (mcdr c) v))
(define (get-next-turns c)
  (mcdr (mcdr c)))
(define (set-next-turns! c v)
  (set-mcdr! (mcdr c) v))

(define U 8)
(define R 4)
(define D 2)
(define L 1)
(define default-next-turns (list 'left 'straight 'right))

(define (has-R? g x y)
  (coord-exists? g (+ x 1) y))
(define (get-R g x y)
  (coord-get g (+ x 1) y))
(define (has-L? g x y)
  (coord-exists? g (- x 1) y))
(define (get-L g x y)
  (coord-get g (- x 1) y))
(define (has-U? g x y)
  (coord-exists? g x (- y 1)))
(define (get-U g x y)
  (coord-get g x (- y 1)))
(define (has-D? g x y)
  (coord-exists? g x (+ y 1)))
(define (get-D g x y)
  (coord-get g x (+ y 1)))
(define (is-U? n)
  (= U (bitwise-and n U)))
(define (is-R? n)
  (= R (bitwise-and n R)))
(define (is-D? n)
  (= D (bitwise-and n D)))
(define (is-L? n)
  (= L (bitwise-and n L)))
(define (is-+? n)
  (= n 15))

(define (transform-cart c)
  (cond [(char=? c #\^) U]
        [(char=? c #\>) R]
        [(char=? c #\v) D]
        [(char=? c #\<) L]
        [else U]))

(define (transform-input-to-map-coord grid line idx y)
  (let ([c (string-ref line idx)])
    (cons (cond [(char=? c #\\) (if (and (has-U? grid idx y) (is-U? (get-U grid idx y))) 12 3)] ; 3 or 12
                [(or (char=? c #\-) (char=? c #\>) (char=? c #\<)) 5]
                [(char=? c #\/) (if (and (has-U? grid idx y) (is-U? (get-U grid idx y))) 9 6)] ; 6 or 9
                [(or (char=? c #\|) (char=? c #\^) (char=? c #\v)) 10]
                [(char=? c #\+) 15]
                [else 0])
          (if (is-cart? c)
              (make-cart idx y (transform-cart c) default-next-turns)
              '()))))

(define (is-cart? c)
  (or (char=? c #\^) (char=? c #\>) (char=? c #\v) (char=? c #\<)))

(define (parse filename)
  (define (parse-line line y grid max-x carts)
    (for ([idx (in-range 0 (string-length line))])
      (let ([found (transform-input-to-map-coord grid line idx y)])
        (if (empty? (cdr found)) '() (set! carts (add-cart! (cdr found) carts)))
        (coord-set! grid idx y (car found))))
    (for ([idx (in-range (string-length line) max-x)])
      (coord-set! grid idx y 0))
    carts)
  (let* ([lines (file->lines filename)]
         [max-x (string-length (argmax string-length lines))]
         [max-y (length lines)]
         [grid (make-grid)]
         [carts (make-carts)])
    (let loop ([lines lines]
               [y 0])
      (if (empty? lines)
          (cons grid (list->vector carts))
          (begin (set! carts (parse-line (car lines) y grid max-x carts))
                 (loop (cdr lines) (+ y 1)))))))

(define (move idx cart current-direction)
  (if (is-+? current-direction)
      (let ([next-turn (car (get-next-turns cart))])
        (if (empty? (cdr (get-next-turns cart)))
            (set-next-turns! cart default-next-turns)
            (set-next-turns! cart (cdr (get-next-turns cart))))
        (set-direction! cart (cond [(eq? next-turn 'left) (if (is-U? (get-direction cart)) L (arithmetic-shift (get-direction cart) 1))]
              [(eq? next-turn 'right) (if (is-L? (get-direction cart)) U (arithmetic-shift (get-direction cart) -1))]
              [else (get-direction cart)])))
      #f)
      (if (= (get-direction cart) (bitwise-and (get-direction cart) current-direction))
          (cond [(is-U? (get-direction cart)) (make-cart (get-position-x cart) (- (get-position-y cart) 1) (get-direction cart) (get-next-turns cart))]
                [(is-R? (get-direction cart)) (make-cart (+ (get-position-x cart) 1) (get-position-y cart) (get-direction cart) (get-next-turns cart))]
                [(is-D? (get-direction cart)) (make-cart (get-position-x cart) (+ (get-position-y cart) 1) (get-direction cart) (get-next-turns cart))]
                [(is-L? (get-direction cart)) (make-cart (- (get-position-x cart) 1) (get-position-y cart) (get-direction cart) (get-next-turns cart))]
                [else cart])
          (move idx cart (turn cart current-direction))))

(define (turn cart current-direction)
  (let* ([modifier (if (or (is-U? (get-direction cart)) (is-R? (get-direction cart))) -2 2)]
         [new-direction (bitwise-xor current-direction (arithmetic-shift (get-direction cart) modifier))])
    (set-direction! cart new-direction)
    new-direction))

(define (find-cart cart carts start-idx)
  (if (= start-idx (vector-length carts))
      #f
      (let ([search-cart (vector-ref carts start-idx)])
        (if (and (= (get-position-x search-cart) (get-position-x cart))
                 (= (get-position-y search-cart) (get-position-y cart)))
            search-cart
            (find-cart cart carts (+ start-idx 1))))))

(define (find-collision carts)
  (let loop ([idx (vector-length carts)])
    (if (= idx 0)
        #f
        (if (find-cart (vector-ref carts (- idx 1)) carts idx)
            (vector-ref carts (- idx 1))
            (loop (- idx 1))))))

(define (sort-carts carts)
  (vector-sort carts string<? #:key (lambda (cart) (string-append (string (integer->char (get-position-x cart))) "x" (string (integer->char (get-position-y cart)))))))

(define (solve grid carts)
  (let loop ([carts (sort-carts carts)])
    (let inner-loop ([idx 0])
      (if (= idx (vector-length carts))
          (loop (sort-carts carts))
          (let* ([cart-to-move (vector-ref carts (modulo idx (vector-length carts)))]
                 [cart-direction (get-direction cart-to-move)]
                 [current-direction (coord-get grid (get-position-x cart-to-move) (get-position-y cart-to-move))])
            (vector-set! carts (modulo idx (vector-length carts)) (move (modulo idx (vector-length carts)) cart-to-move current-direction))
            (let ([collided (find-collision carts)])
              (if collided
                  (string-append (number->string (get-position-x collided)) "," (number->string (get-position-y collided)))
                  (inner-loop (+ idx 1)))))))))

(let* ([parsed (parse "input1.txt")]
       [grid (car parsed)]
       [carts (cdr parsed)])
  (solve grid carts))
