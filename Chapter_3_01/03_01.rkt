#lang sicp

(define (make-accumulator value)
  (lambda (amount)
    (set! value (+ amount value))
    value))
