#lang sicp

(define (for-each proc items)
  (cond ( (null? items)
      "done" )
      (else (proc (car items))
            (for-each proc (cdr items)))))

(define (display-and-newline x)
  (newline)
  (display x)
  )
