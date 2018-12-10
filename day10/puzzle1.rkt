#lang racket
(require racket/gui/base)

(define (make-coord cs vs)
  (mcons (car cs) (mcons (cadr cs) (mcons (car vs) (cadr vs)))))

(define (x c)
  (mcar c))
(define (x! c v)
  (set-mcar! c v))
(define (y c)
  (mcar (mcdr c)))
(define (y! c v)
  (set-mcar! (mcdr c) v))
(define (vx c)
  (mcar (mcdr (mcdr c))))
(define (vy c)
  (mcdr (mcdr (mcdr c))))

(define (parse filename)
  (let ([lines (file->lines filename)])
    (map (lambda (line)
           (make-coord (map (lambda (x) (string->number (string-trim x))) (string-split (string-trim (car (string-split line "> velocity=<")) "position=<") ", "))
                 (map (lambda (x) (string->number (string-trim x))) (string-split (string-trim (cadr (string-split line "> velocity=<")) ">") ", "))))
         lines)))

(define (move-all coords)
  (if (empty? coords)
      '()
      (cons (let ([c (car coords)])
               (make-coord (list (+ (x c) (vx c))
                                 (+ (y c) (vy c)))
                           (list (vx c) (vy c))))
             (move-all (cdr coords)))))

(define (normalize coords)
  (let ([minx (argmin x coords)]
        [maxx (argmax x coords)]
        [miny (argmin y coords)]
        [maxy (argmax y coords)])
    (let loop ([coords coords])
      (if (empty? coords)
          '()
          (cons (let ([c (car coords)])
                  (make-coord (list (/ (- (x c) (x minx)) (- (x maxx) (x minx)))
                                    (/ (- (y c) (y miny)) (- (y maxy) (y miny))))
                              (list (vx c) (vy c))))
                (loop (cdr coords)))))))                              

(define frame (new frame%
                   [label "Example"]
                   [width 1000]
                   [height 1000]))
(define normalized '())
(define counter 0)
(define canvas 
          (new canvas% [parent frame]
             [paint-callback
              (lambda (canvas dc)
                (send dc set-text-foreground "blue")
                (send dc draw-text (number->string counter) 100 500)
                (for-each (lambda (c) (send dc draw-text "x" (+ 10 (* (+ 0.3 (x c)) 600)) (+ 10 (* (+ 0.3 (y c)) 300)))) normalized))]))

(define (solve coords)
  (let loop ([i 0])
   (if (< i 10659)
       (begin
         (set! coords (move-all coords))
         (loop (+ i 1)))
       #f))
  (let loop ([i 0]
             [coords coords])
    (if (or (> i 0) (empty? coords))
        #f
        (let ([new-normalized (normalize coords)])
          (set! counter (+ counter 1))
          (set! normalized new-normalized)
          (send frame refresh)
          (send canvas refresh-now)
          (send canvas on-paint)
          (send frame show #t)
          (sleep 5)
          (loop (+ i 1) (move-all coords))))))

(define parsed-file (parse "input1.txt"))

(solve parsed-file)