#lang racket

(define (average x y)
  (/ (+ x y) 2))

(define (sqrt x)
  (define (sqrt-iter guess)
    (if (good-enough? guess)
        guess
        (sqrt-iter (improve guess))))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (good-enough? guess)
    (= (improve guess) guess))
  (sqrt-iter 1.0))

(sqrt 9)
(sqrt 16)
