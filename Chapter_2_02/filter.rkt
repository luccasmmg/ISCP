#lang sicp

(define (filter predicate items)
  (cond ( (null? items) '())
        ( (predicate (car items)) (cons (car items) (filter predicate (cdr items))))
        ( else (filter predicate (cdr items)) )))

(filter (lambda (x) (> x 3)) (list 1 4 2 7))
