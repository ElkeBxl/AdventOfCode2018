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

(define (filter-prerequisites nodes node)
  (if (empty? nodes)
      '()
      (let ([first-node (car nodes)])
        (cons (cons (letter first-node) (filter (lambda (x) (not (= x (letter node)))) (prerequisites first-node)))
              (filter-prerequisites (cdr nodes) node)))))

(define (solve nodes)
  (list->string (map
   (lambda (x) (integer->char (car x)))
   (let loop ([ordered-nodes (sort nodes node<)])
    (if (empty? ordered-nodes)
        '()
        (let ([cleaned-up-nodes (filter-prerequisites ordered-nodes (car ordered-nodes))])
          (cons (car ordered-nodes)
                (loop (sort (cdr cleaned-up-nodes) node<)))))))))

(solve (fill-up-alphabet (merge (parse "input1.txt"))))