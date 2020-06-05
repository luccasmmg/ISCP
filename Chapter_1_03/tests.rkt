#lang sicp

(define (test x)
  (let ((x 3) (y (+ x 2)))
  (* x y)))

(define (square x)
  (* x x))

(define (f x y)
  (let ((a (+ 1 (* x y)))
        (b (- 1 y)))
    (+ (* x (square a))
       (* y b)
       (* a b))))
