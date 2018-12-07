#lang racket

(define node cons)
(define letter car)
(define prerequisites cdr)
(define (letter< n1 n2)
  (< (letter n1) (letter n2)))
(define (letter= n1 n2)
  (= (letter n1) (letter n2)))
(define (node< n1 n2)
  (if (= (length (prerequisites n1)) (length (prerequisites n2)))
      (letter< n1 n2)
      (< (length (prerequisites n1)) (length (prerequisites n2)))))
(define work cons)
(define time-left car)
(define working-on cdr)

(define (parse filename)
  (let ([file-list (file->lines filename)])
    (sort (map (lambda (a) (node (char->integer (car (string->list (substring a 36 37))))
                                 (list (char->integer (car (string->list (substring a 5 6)))))))
               file-list)
          letter<)))

(define (merge original-list)
  (let loop ([old-list original-list]
             [new-list (list)]
             [previous-node (node 0 (list))])
    (if (empty? old-list)
        (cdr (reverse (cons previous-node new-list)))
        (let ([current-node (car old-list)])
          (if (letter= current-node previous-node)
              (loop (cdr old-list) new-list (node (letter previous-node) (append (prerequisites current-node) (prerequisites previous-node))))
              (loop (cdr old-list) (cons previous-node new-list) current-node))))))

(define (fill-up-alphabet original-nodes)
  (let ([v (make-vector (- (letter (argmax letter original-nodes)) 64) '())])
    (let loop ([nodes original-nodes]
               [idx 0])
      (if (empty? nodes)
          (let inner-loop ([new-letter 65]
                           [new-list (vector->list v)])
            (if (empty? new-list)
                '()
                (cons (node new-letter (vector-ref v (- new-letter 65)))
                      (inner-loop (+ 1 new-letter) (cdr new-list)))))
          (if (= (- (letter (car nodes)) 65) idx)
              (begin (vector-set! v idx (prerequisites (car nodes)))
                     (loop (cdr nodes) (+ 1 idx)))
              (loop nodes (+ 1 idx)))))))

(define (filter-prerequisites nodes node-letter)
  (if (empty? nodes)
      '()
      (let ([first-node (car nodes)])
        (cons (cons (letter first-node) (filter (lambda (x) (not (= x node-letter))) (prerequisites first-node)))
              (filter-prerequisites (cdr nodes) node-letter)))))

(define (can-assign-work? workers)
  (let loop ([idx 0])
    (if (= idx (vector-length workers))
        #f
        (let ([first-worker (vector-ref workers idx)])
          (if (= (time-left first-worker) 0)
              first-worker
              (loop (+ idx 1)))))))

(define (get-first-worker workers)
  (let loop ([idx 0])
    (if (= idx (vector-length workers))
        #f
        (let ([first-worker (vector-ref workers idx)])
          (if (= (time-left first-worker) 0)
              idx
              (loop (+ idx 1)))))))

(define (get-busy-workers workers)
  (let loop ([idx 0])
    (if (= idx (vector-length workers))
        '()
        (let ([first-worker (vector-ref workers idx)])
          (if (and (= (time-left first-worker) 0) (not (working-on first-worker)))
              (loop (+ idx 1))
              (cons first-worker (loop (+ idx 1))))))))

(define (assign-work workers worker node)
  (vector-set! workers worker (work (- (letter node) 4) (letter node))))

(define (decrease-time-left workers)
  (vector-map! (lambda (worker) (work (max (- (time-left worker) 1) 0) (working-on worker))) workers))

(define (get-finished-work workers)
  (let loop ([idx 0])
    (if (= idx (vector-length workers))
        '()
        (let ([first-worker (vector-ref workers idx)])
          (if (and (= (time-left first-worker) 0) (working-on first-worker))
              (begin (vector-set! workers idx (work 0 #f))
                     (cons (working-on first-worker)
                           (loop (+ idx 1))))
              (loop (+ idx 1)))))))

(define (is-work-available? nodes)
  (if (empty? nodes)
      #f
      (= (length (prerequisites (car nodes))) 0)))

(define (get-available-work nodes)
  (if (empty? nodes)
      '()
      (if (= (length (prerequisites (car nodes))) 0)
          (cons (car nodes) (get-available-work (cdr nodes)))
          (get-available-work (cdr nodes)))))

(define (solve nodes workers)
   (let loop ([ordered-nodes (sort nodes node<)]
              [workers (decrease-time-left workers)]
              [result ""]
              [time-spent 0])
    (if (and (empty? ordered-nodes) (empty? (get-busy-workers workers)))
        (cons result (- time-spent 1))
        (let* ([finished-work (get-finished-work workers)]
               [next-nodes ordered-nodes])
          (for-each (lambda (finished) (begin
                                         (set! next-nodes (sort (filter (lambda (x) (not (= (letter x) finished))) (filter-prerequisites ordered-nodes finished)) node<))
                                         (set! result (string-append result (string (integer->char finished))))))
                    finished-work)
              (if (is-work-available? next-nodes)
                  (let ([assigned-work '()])
                    (for-each (lambda (available-work)
                                (if (can-assign-work? workers)
                                    (begin
                                      (assign-work workers (get-first-worker workers) available-work)
                                      (set! assigned-work (cons available-work assigned-work)))
                                    #f))
                              (get-available-work next-nodes))
                    (for-each (lambda (assigned) (set! next-nodes (remove assigned next-nodes letter=))) assigned-work)
                    (loop (sort next-nodes node<) (decrease-time-left workers) result (+ time-spent 1)))
                  (loop (sort next-nodes node<) (decrease-time-left workers) result (+ time-spent 1)))))))

(define workers (make-vector 5 (work 0 #f)))
(solve (fill-up-alphabet (merge (parse "input1.txt"))) workers)