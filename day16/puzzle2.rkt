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
(define all-opcode-tags (list 'addr 'addi 'mulr 'muli 'banr 'bani 'borr 'bori 'setr 'seti 'gtir 'gtri 'gtrr 'eqir 'eqri 'eqrr))
(define candidates (make-vector 16 '()))

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

(define (parse-program filename)
  (let loop ([lines (file->lines filename)])
    (if (empty? lines)
        '()
        (cons (map string->number (string-split (car lines) " "))
              (loop (cdr lines))))))

(define (sort-opcodes-with-input inputs-with-codes)
  (sort inputs-with-codes (lambda (x y) (< (length x) (length y)))))

(define (filter-found-opcode opcode inputs-with-codes)
  (map (lambda (x) (cons (car x) (filter (lambda (x) (not (equal? (cadr opcode) x))) (cdr x)))) inputs-with-codes))

(define (match-opcodes inputs-with-codes)
  (let loop ([inputs-with-codes (sort-opcodes-with-input inputs-with-codes)]
             [result '()])
    (if (empty? inputs-with-codes)
        (sort result < #:key car)
        (loop (sort-opcodes-with-input (filter-found-opcode (car inputs-with-codes) (cdr inputs-with-codes)))
              (cons (car inputs-with-codes) result)))))

(define (execute opcodes-list program)
  (let loop ([opcodes (list->vector (map cadr opcodes-list))]
        [program program])
    (if (empty? program)
        (vector-ref registers 0)
        (let ([opcode (list-ref all-opcodes (index-of all-opcode-tags (vector-ref opcodes (caar program))))])
          (apply opcode (cdar program))
          (loop opcodes (cdr program))))))

(define (solve original-scenarios program)
  (let loop ([scenarios original-scenarios])
    (if (empty? scenarios)
        (execute (match-opcodes (vector->list (vector-map cons (list->vector (list 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)) (vector-map! remove-duplicates candidates)))) program)
        (begin
          (for ([opcode all-opcodes]
                [tag all-opcode-tags])
            (set-registers-values! (get-before (car scenarios)))
            (apply opcode (cdr (get-input (car scenarios))))
            (if (equal? (get-after (car scenarios)) (vector->list registers))
                (vector-set! candidates (car (get-input (car scenarios))) (cons tag (vector-ref candidates (car (get-input (car scenarios))))))
                #f))
          (loop (cdr scenarios))))))

(solve (parse "input1.txt") (parse-program "input2.txt"))