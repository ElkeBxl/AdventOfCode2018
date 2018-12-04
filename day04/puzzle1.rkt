#lang racket

(define month car)
(define day cadr)
(define hour caddr)
(define minutes cadddr)
(define description (lambda (x) (cadddr (cdr x))))

(define (parse filename)
  (map (lambda (x) (list (substring x 6 8)
                         (substring x 9 11)
                         (substring x 12 14)
                         (substring x 15 17)
                         (substring x 19)))
       (sort (file->lines filename) string<?)))

(define (get-guard-id entry)
  (string->number (string-trim (string-trim (description entry) "Guard #" #:right? #f) " begins shift" #:left? #f)))

(define (is-begin-shift-entry? entry)
  (string-contains? (description entry) "begins shift"))

(define (is-wakes-up-entry? entry)
  (string-contains? (description entry) "wakes up"))

(define (is-falls-asleep-entry? entry)
  (string-contains? (description entry) "falls asleep"))

(define (register-sleep entries result current-guard)
  (if (empty? entries)
      (hash->list result)
      (let ([entry (car entries)])
        (if (is-begin-shift-entry? entry)
            (register-sleep (cdr entries) result (get-guard-id entry))
            (begin
              (if (is-falls-asleep-entry? entry)
                  (if (hash-has-key? result current-guard)
                      (hash-set! result current-guard (cons (minutes entry) (hash-ref result current-guard)))
                      (hash-set! result current-guard (list (minutes entry))))
                  (if (hash-has-key? result current-guard)
                      (hash-set! result current-guard (cons (minutes entry) (hash-ref result current-guard)))
                      (hash-set! result current-guard (list (minutes entry)))))
              (register-sleep (cdr entries) result current-guard))))))                  

(define (expand-time entries vector)
  (if (empty? entries)
      vector
      (begin
        (for ([range (in-range (string->number (car entries)) (string->number (cadr entries)))])
        (vector-set! vector range (+ (vector-ref vector range) 1)))
        (expand-time (cddr entries) vector))))

(define (calculate-times guards-entries result)
  (if (empty? guards-entries)
      result
      (let ([guard-entries (reverse (cdar guards-entries))]
            [guard-id (caar guards-entries)])
        (displayln guard-id)
        (displayln guard-entries)
      (calculate-times (cdr guards-entries) (cons (cons guard-id (expand-time guard-entries (make-vector 60))) result)))))

(define (find-max-in-vector vector idx max-idx max-value total-sleep)
  (if (= idx (vector-length vector))
      (cons max-idx total-sleep)
      (if (> (vector-ref vector idx) max-value)
          (find-max-in-vector vector (+ 1 idx) idx (vector-ref vector idx) (+ total-sleep (vector-ref vector idx)))
          (find-max-in-vector vector (+ 1 idx) max-idx max-value (+ total-sleep (vector-ref vector idx))))))

(define (find-max entries previous-max id)
  (if (empty? entries)
      (* (car previous-max) id)
      (let ([found-max (find-max-in-vector (cdar entries) 0 0 0 0)])
        (displayln found-max)
        (if (> (cdr found-max) (cdr previous-max))
            (find-max (cdr entries) found-max (caar entries))
            (find-max (cdr entries) previous-max id)))))

(define (solve lines)
  (find-max (calculate-times (register-sleep lines (make-hash) 0) (list)) (cons 0 0) 0))

(define parsed-file (parse "input1.txt"))
(solve parsed-file)