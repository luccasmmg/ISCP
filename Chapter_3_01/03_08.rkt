#lang sicp

(define z 0)

(define (f x)
  (if (= z 1)
      0
      (begin (set! z x) z)))
