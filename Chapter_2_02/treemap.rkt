#lang sicp

(define (square x) (* x x))

(define (treemap f items)
  (cond ((null? items) nil)
        ((not (pair? items)) (f items))
        (else (cons (treemap f (car items)) (treemap f (cdr items))))))

(define (treemap-2 f tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (treemap-2 f sub-tree)
             (f sub-tree)))
       tree))

(define (treemap-3 f tree)
  (cons (f (car tree))
        (map (lambda (child) (treemap f child))
             (cdr tree)) ))

(define (square-tree tree)
  (treemap-3 square tree))
