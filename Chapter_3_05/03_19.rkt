#lang sicp

(define (last-pair items)
(let ((rest (cdr items)))
    (if (null? rest)
        items
        (last-pair rest))))

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

 (define (contains-cycle? lst)
   (define (safe-cdr l)
     (if (pair? l)
         (cdr l)
         '()))
   (define (iter a b)
     (cond ((not (pair? a)) #f)
           ((not (pair? b)) #f)
           ((eq? a b) #t)
           ((eq? a (safe-cdr b)) #t)
           (else (iter (safe-cdr a) (safe-cdr (safe-cdr b))))))
   (iter (safe-cdr lst) (safe-cdr (safe-cdr lst))))
