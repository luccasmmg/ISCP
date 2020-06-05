#lang sicp

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (average x y)
  (newline)
  (display y)
  (display "---->")
  (display x)
  (/ (+ x y) 2))

(define (sqrt x)
  (fixed-point
   (lambda (y) (average y (/ x y)))
   1.0))

(define (x*x=1000 x)
  (fixed-point
   (lambda (x) (/ (log 1000) (log x)))
   x))

(define (x*x=1000_with_damping x)
  (fixed-point
   (lambda (x) (average x (/ (log 1000) (log x))))
   x))
