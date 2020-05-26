#lang racket
(define (fast-expt b n)
  (fast-expt-iter b n (square b)))

(define (square b)
  (* b b))

(define (fast-expt-iter b n a)
  (if (= n 2)
      a
      (fast-expt-iter
       b (/ n 2) (square a))))

(fast-expt 3 16)
