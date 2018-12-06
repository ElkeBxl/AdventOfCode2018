#lang racket

(define coord cons)
(define x car)
(define y cdr)
(define (get-key c)
  (string-append (number->string (x c)) "x" (number->string (y c))))
(define (coord= c1 c2)
  (and (= (x c1) (x c2))
       (= (y c1) (y c2))))

(define (coord-x< c1 c2)
  (< (x c1) (x c2)))

(define (coord-x> c1 c2)
  (> (x c1) (x c2)))

(define (coord-y< c1 c2)
  (< (y c1) (y c2)))

(define (coord-y> c1 c2)
  (> (y c1) (y c2)))

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

(define (is-boxed-in? c grid greatest-x greatest-y lowest-x lowest-y)
  (and (for*/and ([x (in-range lowest-x (+ greatest-x 1))]
            [y (list lowest-y greatest-y)])
         (let* ([new-coord (coord x y)]
                [new-key (get-key new-coord)])
           (and (not (coord= c new-coord)) (or (> (length (hash-ref grid new-key)) 1) (not (member c (hash-ref grid new-key) coord=))))))
      (for*/and ([x (list lowest-x greatest-x)]
            [y (in-range lowest-y (+ greatest-y 1))])
         (let* ([new-coord (coord x y)]
                [new-key (get-key new-coord)])
           (and (not (coord= c new-coord)) (or (> (length (hash-ref grid new-key)) 1) (not (member c (hash-ref grid new-key) coord=))))))))

(define (search-greatest-area grid original-coords)
  (let ([greatest-x (find-greatest x original-coords)]
        [greatest-y (find-greatest y original-coords)]
        [lowest-x (find-lowest x original-coords)]
        [lowest-y (find-lowest y original-coords)])
    (let loop ([coords (filter (lambda (x) (is-boxed-in? x grid greatest-x greatest-y lowest-x lowest-y)) original-coords)]
               [occurences (list)])
      (if (empty? coords)
          (let filter-loop ([occurences occurences]
                            [most-found (car occurences)])
            (if (empty? occurences)
                most-found
                (if (> (cdar occurences) (cdr most-found))
                    (filter-loop (cdr occurences) (car occurences))
                    (filter-loop (cdr occurences) most-found))))
          (let ([times-occuring 0])
            (for* ([idx-x (in-range lowest-x (+ greatest-x 1))]
                   [idx-y (in-range lowest-y (+ greatest-y 1))])
              (let* ([new-coord (coord idx-x idx-y)]
                     [new-key (get-key new-coord)]
                     [current-key (get-key (car coords))]
                     [current-coords (hash-ref grid new-key)])
                (cond [(and (= (length current-coords) 1) (coord= (car coords) (car current-coords)))
                       (set! times-occuring (+ times-occuring 1))])))
            (loop (cdr coords) (cons (cons (car coords) times-occuring) occurences)))))))

(define (solve original-coords)
  (let ([greatest-x (find-greatest x original-coords)]
        [greatest-y (find-greatest y original-coords)]
        [lowest-x (find-lowest x original-coords)]
        [lowest-y (find-lowest y original-coords)]
        [grid (make-hash)])
    (let loop ([coords original-coords])
      (if (empty? coords)
          (search-greatest-area grid original-coords)
          (let* ([c (car coords)]
                 [key (get-key c)])
            (for* ([idx-x (in-range lowest-x (+ greatest-x 1))]
                   [idx-y (in-range lowest-y (+ greatest-y 1))])
              (let* ([new-coord (coord idx-x idx-y)]
                     [new-key (get-key new-coord)])
                (if (hash-has-key? grid new-key)
                    (let* ([current (hash-ref grid new-key)]
                           [new-distance (calc-manhattan-distance new-coord c)]
                           [old-distance (calc-manhattan-distance new-coord (car current))])
                      (cond [(< new-distance old-distance) (hash-set! grid new-key (list c))]
                            [(= new-distance old-distance) (hash-set! grid new-key (cons c current))]))
                    (hash-set! grid new-key (list c)))))
            (loop (cdr coords)))))))

(solve (parse "input1.txt"))

