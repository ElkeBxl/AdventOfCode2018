#lang racket
(define (solve input)
  (define recipes (make-vector 1000000 1))
  (vector-set! recipes 0 3)
  (vector-set! recipes 1 7)
  (let loop ([amount (+ input 10)]
             [recipes recipes]
             [elf1 0]
             [elf2 1]
             [recipes-length 2])
    (if (>= recipes-length amount)
        (vector-drop (vector-take recipes recipes-length) input)
        (let* ([elf1-moves (vector-ref recipes elf1)]
               [elf2-moves (vector-ref recipes elf2)]
               [next-sum (+ (vector-ref recipes elf1) (vector-ref recipes elf2))]
               [next-sums (if (> next-sum 9)
                              (list 1 (modulo next-sum 10))
                              (list next-sum))])
          (for ([i (in-range 0 (length next-sums))])
            (vector-set! recipes recipes-length (list-ref next-sums i))
            (set! recipes-length (+ recipes-length 1)))
          (loop amount recipes (modulo (+ 1 elf1 elf1-moves) recipes-length) (modulo (+ 1 elf2 elf2-moves) recipes-length) recipes-length)))))

(solve 920831)
        