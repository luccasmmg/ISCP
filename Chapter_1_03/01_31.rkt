#lang sicp

(define (product term a next b)
  (if (> a b)
      1
      (* (term a) (product term (next a) next b))))

(define (product-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
  (iter a 1))

(define (identity x) x)

(define (inc x) (+ 1 x))

(define (factorial x)
  (product identity 1 inc x))

(define (factorial-iter x)
  (product-iter identity 1 inc x))
