#lang sicp

(define (square x) (* x x))

;; (define (square-list items)
;;   (if (null? items)
;;       nil
;;       (cons (square (car items)) (square-list (cdr items) ))
;;       ))

(define (map proc items)
  (if (null? items)
      '()
      (cons (proc (car items))
            (map proc (cdr items)))))

(define (square-list items)
  (map square items))
