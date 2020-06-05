#lang sicp

(define (cons x y)
  (lambda (m) (m x y)))

;(define x (cons 1 2))
;Is equal to (lambda (m) (m 1 2))


(define (car z)
  (z (lambda (p q) p)))
;(car z)
;Is equal to (lambda (m) (m 1 2)) with the argument (lambda (p q) p)
;Which evaluates to (lambda (p q) p) inserted into the m in (m 1 2)
;This is is equal as (lambda (1 2) 1)

(define (cdr z)
  (z (lambda (p q) q)))
