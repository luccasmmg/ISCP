#lang racket
(define (double x) (+ x x))
(define (halve x) (/ x 2))

(define (fast-mult b n)
  (cond ((= n 0)
         1)
        ((even? n)
         (double (fast-mult b (halve n))))
        (else
         (+ b (fast-mult b (- n 1))))))

(* 3 7)
