#lang sicp

(define (random-update x) (+ 1 x))

(define random-init 1)
(define rand
  (let (( value random-init ))
    (lambda (message)
      (cond ((eq? message 'generate) (begin
                                       (set! value (random-update value))
                                       value))
            ((eq? message 'reset) (lambda (x)
                                    (set! value x)
                                    x))))))
