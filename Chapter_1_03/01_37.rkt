#lang sicp

(define (cont-frac n d k)
  (if (= k 0)
      0
      (/ (n k) (+ (d k) (cont-frac n d (- k 1))))))

(define (cont-frac-iter n d k)
  (define (iter i result)
    (if (= i k)
        result
        (iter (+ 1 i) (/ (n k) (+ result (d k))))))
  (iter 0 0))

(cont-frac-iter (lambda (i) 1.0)
           (lambda (i) 1.0)
           12)
