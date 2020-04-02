#lang racket

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (square x)
  (* x x))

(define (good-enough? guess x)
  (and (> (abs (/ (* guess 100) (improve guess x))) 99.9999)
       (< (abs (/ (* guess 100) (improve guess x))) 100.0001)))

(define (sqrt x)
  (sqrt-iter 1.0 x))

(sqrt 0.000003)
