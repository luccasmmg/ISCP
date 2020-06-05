#lang sicp

(define (compose f g)
  (lambda (x) (f (g x))))

(define (square x) (* x x))

(define (repeated f n)
     (if (= n 0)
         (lambda (x) x)
         (compose f (repeated f (- n 1))))
)

(define (average-3 x y z) (/ (+ x y z) 3))

(define (smooth f)
  (define dx 0.0001)
  (lambda (x) (average-3 (f (- x dx)) (f x) (f (+ x dx)))))

(define (smooth-n-time f n)
  (repeated (smooth f) n))
