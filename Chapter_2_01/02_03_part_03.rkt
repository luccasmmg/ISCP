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

;Can change
(define (make-rectangle x)
  x)
(define (get-width x)
  (x-point x))

(define (get-height x)
  (y-point x))

(define x (make-rectangle (make-point 4.0 5.0)))

;Cant change
(define (area x)
  (* (get-width x) (get-height x)))

(define (perimeter x)
  (+ (* (get-width x) 2) (* (get-height x) 2)))
