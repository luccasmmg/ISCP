#lang sicp

(define (equal? x y)
  (cond ((and (symbol? x) (symbol? y)) (eq? x y))
        ((and (pair? x) (pair? y)) (and (eq? (car x) (car y)) (equal? (cdr x) (cdr y))))
        ((and (null? x) (null? y)) #t)
        (else false)))
