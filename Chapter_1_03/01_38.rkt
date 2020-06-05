#lang sicp

(define (cont-frac n d k)
  (if (= k 0)
      0
      (/ (n k) (+ (d k) (cont-frac n d (- k 1))))))

(define (cont-frac-iter n d k)
  (define (iter k result)
    (if (= k 0)
      result
      (iter (- k 1) (/ (n k) (+ (d k) result)))))
  (iter k 0))

(define (k? x)
  (let ((k (+ x 1.0)))
    (if (= (remainder k 3.0) 0.0)
        (* (/ k 3.0) 2.0)
        1.0)))

(cont-frac-iter (lambda (i) 1.0) k? 12)
