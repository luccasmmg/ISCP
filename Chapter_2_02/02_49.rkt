#lang sicp

(define (make-vect x y) (cons x y))
(define 0-0 (make-vect 0 0))
(define 0-1 (make-vect 0 1))
(define 1-1 (make-vect 1 1))
(define 1-0 (make-vect 1 0))
;a
(segments->painter (list
                    (make-segment 0-0 0-1)
                    (make-segment 0-1 1-1)
                    (make-segment 1-1 1-0)
                    (make-segment 1-0 0-0)))
;b
(segments->painter (list
                    (make-segment 0-0 1-1)
                    (make-segment 1-0 0-1)))
;c
(define x (make-vect 0.0 0.5))
(define y (make-vect 0.5 1))
(define z (make-vect 1 0.5))
(define k (make-vect 0.5 0))
(segments->painter (list
                    (make-segment x y)
                    (make-segment y z)
                    (make-segment z k)
                    (make-segment k x)))
