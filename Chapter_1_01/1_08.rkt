#lang racket

(define (square x)
  (* x x))

(define (cube-root x)
  (define (cube-root-iter guess)
    (if (good-enough? guess)
        guess
        (cube-root-iter (improve guess))))
  (define (improve guess)
    (/ (+ (divided-by-square guess) (* guess 2)) 3))
  (define (divided-by-square guess)
    (/ x (square guess)))
  (define (good-enough? guess)
    (= (improve guess) guess))
  (cube-root-iter 1.0))

(cube-root 8)
