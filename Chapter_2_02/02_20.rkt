#lang sicp

;; (define (same-parity x . z)
;;   (let (( test (even-odd-parity x) ) ( new-z (cdr z) ))
;;     (cond ((null? z) '())
;;           ((test (car remainder)) (append (same-parity x . new-z ) (car (list new-z))))
;;           (else (same-parity x . (cdr z)))
;;           ))
;;   )
;;
(define (same-parity x . y)
  (define (same-parity-iter integer items)
    (let (( test (even-odd-parity x) ))
      (cond ( (null? items) '() )
            ( (test (car items)) (append (list (car items)) (same-parity-iter x (cdr items)) ) )
            ( else (same-parity-iter x (cdr items)) ))
  ))
  (append (list x) (same-parity-iter x y) ))

(define (even-odd-parity base-number)
  (lambda (x) (= (remainder x 2) (remainder base-number 2))))
