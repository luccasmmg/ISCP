#lang racket
(define (square x)
  (* x x))

(define (sum-int a b)
  (if (> a b)
      0
      (+ (square a)
         (sum-int (+ 1 a) b))))

(sum-int 3 8)
