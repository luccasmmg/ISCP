#lang sicp

(define (adjoin-set x set)
  (define (iter first-elements rest)
    (cond ((null? rest) (append first-elements (list x)))
          ((= x (car rest)) set)
          ((< x (car rest)) (append first-elements (list x) rest))
          (else (iter (append first-elements (list (car rest))) (cdr rest)))))
  (iter '() set))
