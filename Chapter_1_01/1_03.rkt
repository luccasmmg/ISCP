#lang racket

(define (square x)
  (* x x))

(define (sum-of-square x y)
  (+ (square x) (square y)))

(define (sum-of-square-two-larger-numbers x y z)
  (cond ((and (> x z) (> y z)) (sum-of-square x y))
        ((> y x) (sum-of-square z y))
        (else (sum-of-square z x)))
  )

(sum-of-square-two-larger-numbers 3 4 5)
(sum-of-square-two-larger-numbers 3 5 4)
(sum-of-square-two-larger-numbers 4 3 5)
(sum-of-square-two-larger-numbers 4 5 3)
(sum-of-square-two-larger-numbers 5 4 3)
(sum-of-square-two-larger-numbers 5 3 4)
