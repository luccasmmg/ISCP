#lang sicp

(define (make-interval a b) (cons a b))

(define (lower-bound x)
  (min (car x) (cdr x)))

(define (upper-bound x)
  (max (car x) (cdr x)))

(define (make-center-percent c p)
  (make-interval
   (- c (/ (* c p) 100))
   (+ c (/ (* c p) 100))))

(define (center i)
  (/ (+ (lower-bound i)
        (upper-bound i))
     2))

(define (percent i)
  (- 100
     (/ (* 100 (lower-bound i))
        (center i))))
