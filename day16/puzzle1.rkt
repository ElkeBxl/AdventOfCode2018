#lang racket

(define registers (make-vector 4 0))

(define (set-registers-values! l)
  (let loop ([idx 0]
             [rest-l l])
    (if (empty? rest-l)
        #f
        (begin (vector-set! registers idx (car rest-l))
               (loop (+ idx 1) (cdr rest-l))))))

(define (addr a b c)
  (vector-set! registers c (+ (vector-ref registers a) (vector-ref registers b))))

(define (addi a b c)
  (vector-set! registers c (+ (vector-ref registers a) b)))

(define (mulr a b c)
  (vector-set! registers c (* (vector-ref registers a) (vector-ref registers b))))

(define (muli a b c)
  (vector-set! registers c (* (vector-ref registers a) b)))

(define (banr a b c)
  (vector-set! registers c (bitwise-and (vector-ref registers a) (vector-ref registers b))))

(define (bani a b c)
  (vector-set! registers c (bitwise-and (vector-ref registers a) b)))

(define (borr a b c)
  (vector-set! registers c (bitwise-ior (vector-ref registers a) (vector-ref registers b))))

(define (bori a b c)
  (vector-set! registers c (bitwise-ior (vector-ref registers a) b)))

(define (setr a b c)
  (vector-set! registers c (vector-ref registers a)))

(define (seti a b c)
  (vector-set! registers c a))

(define (gtir a b c)
  (vector-set! registers c (if (> a (vector-ref registers b)) 1 0)))

(define (gtri a b c)
  (vector-set! registers c (if (> (vector-ref registers a) b) 1 0)))

(define (gtrr a b c)
  (vector-set! registers c (if (> (vector-ref registers a) (vector-ref registers b)) 1 0)))

(define (eqir a b c)
  (vector-set! registers c (if (eq? a (vector-ref registers b)) 1 0)))

(define (eqri a b c)
  (vector-set! registers c (if (eq? (vector-ref registers a) b) 1 0)))

(define (eqrr a b c)
  (vector-set! registers c (if (eq? (vector-ref registers a) (vector-ref registers b)) 1 0)))  

(define all-opcodes (list addr addi mulr muli banr bani borr bori setr seti gtir gtri gtrr eqir eqri eqrr))

(define (make-scenario before input after)
  (mcons before (mcons input after)))

(define (get-before s)
  (mcar s))

(define (get-input s)
  (mcar (mcdr s)))

(define (get-after s)
  (mcdr (mcdr s)))

(define (parse filename)
  (let loop ([lines (file->lines filename)]
             [parsed '()])
    (if (empty? lines)
        parsed
        (if (string=? (car lines) "")
            (loop (cdr lines)
                  parsed)
            (loop (cdddr lines)
                  (cons (make-scenario
                         (map string->number (string-split (string-trim (string-trim (car lines) "Before: [") "]") ", "))
                         (map string->number (string-split (cadr lines) " "))
                         (map string->number (string-split (string-trim (string-trim (caddr lines) "After:  [") "]") ", ")))
                        parsed))))))

(define (sort-opcodes-with-input inputs-with-codes)
  (sort inputs-with-codes (lambda (x y) (< (length x) (length y)))))

(define (filter-found-opcode opcode inputs-with-codes)
  (filter (lambda (x) (not (= (caaar x) (caaar opcode)))) inputs-with-codes))

(define (match-opcodes inputs-with-codes)
  (let loop ([opcodes (sort-opcodes-with-input inputs-with-codes)])
    (if (empty? opcodes)
        #f
        (if (> (length (car opcodes)) 1)
            (begin
              (displayln (car opcodes))
              (loop (sort-opcodes-with-input (filter-found-opcode (car opcodes) (cdr opcodes)))))
            (begin
              (displayln (car opcodes))
              (loop (sort-opcodes-with-input (filter-found-opcode (car opcodes) (cdr opcodes)))))))))

(define (solve original-scenarios)
  (let loop ([scenarios original-scenarios]
             [results '()])
    (if (empty? scenarios)
        (length (filter (lambda (x) (>= (length x) 3))
                        (for/list ([result results])
                          (filter (lambda (x) (car x))
                                  result))))
        (loop (cdr scenarios)
              (cons (for/list ([opcode all-opcodes])
                      (set-registers-values! (get-before (car scenarios)))
                      (apply opcode (cdr (get-input (car scenarios))))
                      (cons (equal? (get-after (car scenarios)) (vector->list registers))
                            (cons (get-input (car scenarios)) opcode)))
                    results)))))

(solve (parse "input1.txt"))