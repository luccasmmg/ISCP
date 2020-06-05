#lang sicp

(define (make-point x y)
  (cons x y))
(define (x-point x)
  (car x))
(define (y-point x)
  (cdr x))

(define (make-segment x y)
  (cons x y))
(define (start-segment x)
  (car x))
(define (end-segment x)
  (cdr x))

(define (average x y)
  (/ (+ x y) 2))

(define (midpoint-segment x)
  (make-point
   (average (x-point (start-segment x)) (x-point (end-segment x)))
   (average (y-point (start-segment x)) (y-point (end-segment x)))
   ))

(define lower-point (make-point 2.0 3.0))
(define upper-point (make-point 8.0 6.0))
(define line (make-segment lower-point upper-point))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))
