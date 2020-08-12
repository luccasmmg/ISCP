#lang sicp

(define (last-pair items)
(let ((rest (cdr items)))
    (if (null? rest)
        items
        (last-pair rest))))

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

 (define (cycle? x)
   (define visited nil)
   (define (iter x)
     (set! visited (cons x visited))
     (display visited)
     (newline)
     (cond ((null? (cdr x)) false)
           ((memq (cdr x) visited) true)
           (else (iter (cdr x)))))
   (iter x))
