#lang racket
(define (double x) (+ x x))
(define (halve x) (/ x 2))

(define (iter-fast-mult b n)
  (define (iter N B A)
    (cond ((= 0 N) A)
          ((even? N) (iter (halve N) (double B) A))
          (else (iter (- N 1) B (+ B A)))))
  (iter n b 0))

(iter-fast-mult 3 8)
