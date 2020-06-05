#lang sicp

(define (last-pair items)
  (if (null? (cdr items))
      (car items)
      (last-pair (cdr items)))
  )
