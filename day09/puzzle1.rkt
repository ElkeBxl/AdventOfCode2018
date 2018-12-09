#lang racket

(define (parse filename)
  (let ([line (car (file->lines filename))])
    (list (string->number (car (string-split line " players; last marble is worth ")))
          (string->number (string-trim (cadr (string-split line " players; last marble is worth ")) " points")))))

(define list-with-current-tag 'linked-list-with-current)
 
(define (make-list-node val prev next)
  (mcons val (mcons prev next)))
(define (list-node-val node)
  (mcar node))
(define (list-node-val! node val)
  (set-mcar! node val))
(define (list-node-next node)
  (mcdr (mcdr node)))
(define (list-node-next! node next)
  (set-mcdr! (mcdr node) next))
(define (list-node-prev node)
  (mcar (mcdr node)))
(define (list-node-prev! node prev)
  (set-mcar! (mcdr node) prev))
 
(define (make size first last current)
  (mcons list-with-current-tag (mcons size (mcons first (mcons last current)))))
(define (size ring)
  (mcar (mcdr ring)))
(define (size! ring size)
  (set-mcar! (mcdr ring) size))
(define (head ring)
  (mcar (mcdr (mcdr ring))))
(define (head! ring node)
  (set-mcar! (mcdr (mcdr ring)) node))
(define (rear ring)
  (mcar (mcdr (mcdr (mcdr ring)))))
(define (rear! ring node)
  (set-mcar! (mcdr (mcdr (mcdr ring))) node))
(define (current ring)
  (mcdr (mcdr (mcdr (mcdr ring)))))
(define (current! ring node)
  (set-mcdr! (mcdr (mcdr (mcdr ring))) node))
 
(define (new)
  (make 0 '() '() '()))
 
(define (set-current-to-first! ring)
  (current! ring (head ring))
  ring)
 
(define (set-current-to-last! ring)
  (current! ring (rear ring))
  ring)
 
(define (current-has-next? ring)
  (not (null? (list-node-next (current ring)))))
 
(define (current-has-previous? ring)
  (not (null? (list-node-prev (current ring)))))
 
(define (set-current-to-next! ring)
  (current!
   ring 
   (list-node-next (current ring)))
  ring)
 
(define (set-current-to-previous! ring)
  (current!
   ring 
   (list-node-prev (current ring)))
  ring)

(define (shift-forward! ring)
  (if (not (current-has-next? ring))
      (set-current-to-first! ring)
      (set-current-to-next! ring))
  ring)

(define (shift-backward-multiple! ring amount)
  (if (> amount 0)
      (begin
        (if (not (current-has-previous? ring))
          (set-current-to-last! ring)
          (set-current-to-previous! ring))
        (shift-backward-multiple! ring (- amount 1)))
      ring))  
 
(define (has-current? ring)
  (not (null? (current ring))))
 
(define (peek ring)
  (list-node-val (current ring)))
 
(define (add! ring val)
  (define curr (current ring))
  (define node '())
  (set! node (make-list-node val curr '()))
  (if (null? curr)
      (head! ring node)
      (list-node-next! node (list-node-next curr)))
  (if (or (null? curr) (eq? (rear ring) curr))
      (rear! ring node)
      (list-node-prev! (list-node-next curr) node))
  (if (not (null? curr))                                 
      (list-node-next! curr node)
      #f)
  (current! ring node)
  (size! ring (+ (size ring) 1))
  ring)
 
(define (delete! ring)
  (define curr (current ring))
  (if (null? (list-node-prev curr)) ; remove first
      (head! ring (list-node-next curr))
      (list-node-next! (list-node-prev curr) 
                       (list-node-next curr)))
  (if (null? (list-node-next curr)) ; remove last
      (rear! ring (list-node-prev curr))
      (list-node-prev! (list-node-next curr)
                       (list-node-prev curr)))
  (size! ring (- (size ring) 1))
  (current! ring (list-node-next curr))
  ring)

(define parsed-file (parse "input1.txt"))

(define (find-max v)
  (argmax max (vector->list v)))

(define (solve amount-of-players highest-marble-value)
  (let ([players (make-vector amount-of-players 0)]
        [r (new)])
    (add! r 0)
    (let loop ([current-marble-value 1]
               [current-player 0])
      (if (> current-marble-value highest-marble-value)
          (find-max players)
          (begin
            (shift-forward! r)
            (if (= (modulo current-marble-value 23) 0)
                (begin
                  (shift-backward-multiple! r 8)
                  (vector-set! players current-player (+ (vector-ref players current-player) current-marble-value (list-node-val (current r))))
                  (delete! r))
                (add! r current-marble-value))
            (loop (+ current-marble-value 1) (modulo (+ current-player 1) amount-of-players)))))))

; Part 1
(solve (car parsed-file) (cadr parsed-file))

; Part 2
(solve (car parsed-file) (* (cadr parsed-file) 100))
          