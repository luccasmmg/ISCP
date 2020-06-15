#lang racket

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
      (cons x set))

(define (intersection-set set1 set2)
  (union-set
   (filter (lambda(x) (element-of-set? x set1)) set2)
   (filter (lambda(x) (element-of-set? x set2)) set1)))

(define (union-set set1 set2)
  (append set1 set2))
