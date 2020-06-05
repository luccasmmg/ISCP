#lang sicp

(define (square x) (* x x))

(define (square-tree tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (square-tree sub-tree)
             (square sub-tree)))
       tree))

(define (square-tree-2 tree )
  (cond ((null? tree) nil)
        ((not (pair? tree))
         (square tree))
        (else
         (cons (square-tree-2 (car tree))
               (square-tree-2 (cdr tree))))))
