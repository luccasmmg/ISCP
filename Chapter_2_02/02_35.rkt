#lang sicp

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op
                      initial
                      (cdr sequence)))))

(define x (list (list 1 2) (list 3 4)))

(define (count-leaves t)
  (accumulate + 0 (map (lambda(children) (if (pair? children) (count-leaves children) 1)) t)))
