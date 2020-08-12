#lang sicp

(define (make-monitored f)
  (define counter 0)
  (define (run-f x)
    (set! counter (+ 1 counter))
    (f x))
  (define (dispatch m)
    (cond ((eq? m 'how-many-calls) counter)
          ((eq? m 'reset-count) (set! counter 0))
          (else (run-f m))))
  dispatch)
