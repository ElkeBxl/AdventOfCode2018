#lang racket

(define (solve input)
  (define recipes (make-vector 25000000 1))
  (define (found-in-vector match recipes-length)
    (if (for/and ([i (in-range 0 (vector-length match))])
          (= (vector-ref recipes (- recipes-length 2 i)) (vector-ref match (- (vector-length match) 1 i)))) ; to test if we have to skip last recipe
        1
        (if (for/and ([i (in-range 0 (vector-length match))])
              (= (vector-ref recipes (- recipes-length 1 i)) (vector-ref match (- (vector-length match) 1 i)))) ; to test with last recipe included
            0
            -1)))
  (vector-set! recipes 0 3)
  (vector-set! recipes 1 7)
  (let loop ([match (list->vector (map (lambda (x) (- (char->integer x) 48)) (string->list (number->string input))))]
             [elf1 0]
             [elf2 1]
             [recipes-length 2])
    (if (and (<= (vector-length match) recipes-length)
             (>= (found-in-vector match recipes-length) 0))
        (- recipes-length (found-in-vector match recipes-length) (vector-length match))
        (let* ([elf1-moves (vector-ref recipes elf1)]
               [elf2-moves (vector-ref recipes elf2)]
               [next-sum (+ (vector-ref recipes elf1) (vector-ref recipes elf2))]
               [next-sums (if (> next-sum 9)
                              (list 1 (modulo next-sum 10))
                              (list next-sum))])
          (for ([i (in-range 0 (length next-sums))])
            (vector-set! recipes recipes-length (list-ref next-sums i))
            (set! recipes-length (+ recipes-length 1)))
          (loop match (modulo (+ 1 elf1 elf1-moves) recipes-length) (modulo (+ 1 elf2 elf2-moves) recipes-length) recipes-length)))))

(solve 920831)
        