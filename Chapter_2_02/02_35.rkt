#lang sicp

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op
                      initial
                      (cdr sequence)))))

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate
                       (cdr sequence))))
        (else (filter predicate
                      (cdr sequence)))))

(define x (list (list 1 2) (list 3 4)))
 (define (count-leaves t)
   (accumulate + 0 (map (lambda (node)
                          (if (pair? node)
                              (count-leaves node)
                              1))
                        t)))
