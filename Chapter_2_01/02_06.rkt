#lang sicp

(define (square x) (* x x))

(define zero (lambda (f) (lambda (x) x)))
(define (zero-2 f)
  (lambda (x) x))
(define one (lambda (f) (lambda (x) (f x))))
(define (one-2 f)
  (lambda (x) (f x)))
(define two (lambda (f) (lambda (x) (f (f x)))))
(define (two-2 f)
  (lambda (x) (f (f x))))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

;(lambda (f) (lambda (x) (f ((zero f) x)))))
;(lambda (f) (lambda (x) (f ((zero f) x)))))
