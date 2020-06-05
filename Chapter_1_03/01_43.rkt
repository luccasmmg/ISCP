#lang sicp

(define (compose f g)
  (lambda (x) (f (g x))))

(define (square x) (* x x))

 (define (repeated f n)
     (if (= n 0)
         (lambda (x) x)
         (compose f (repeated f (- n 1))))
)
