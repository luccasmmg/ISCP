#lang sicp
;Define reverse iteractively

(define (reverse items)
  (define (iter in out)
    (if (null? in)
        out
        (iter (cdr in) (cons (car in) out))))
  (iter items '())
  )
