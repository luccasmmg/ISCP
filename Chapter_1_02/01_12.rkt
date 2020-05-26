#lang racket
(define (p-rec y x)
  (if (or (= x 1) (= x y))
      1
      (+
       (p-rec (- y 1) (- x 1))
       (p-rec (- y 1) x)
       )))
(p-rec 5 4)
