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

(define (make-rectangle w h)
  (cons w h))

(define start-width (make-point 0.0 0.0))
(define end-width (make-point 4.0 0.0))

(define start-height (make-point 0.0 0.0))
(define end-height (make-point 0.0 5.0))

(define width (make-segment start-width end-width))
(define height (make-segment start-height end-height))

(define x (make-rectangle width height))

(define (get-width x)
  (- (x-point (end-segment (car x))) (x-point (start-segment (car x)))))

(define (get-height x)
  (- (y-point (end-segment (cdr x))) (y-point (start-segment (cdr x)))))

(define (area x)
  (* (get-width x) (get-height x)))

(define (perimeter x)
  (+ (* (get-width x) 2) (* (get-height x) 2)))
