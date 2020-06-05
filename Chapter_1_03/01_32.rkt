#lang sicp

(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a) (accumulate combiner null-value term (next a) next b))))

(define (accumulate-iter combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner result (term a)))))
  (iter a null-value))

(define (product term a next b)
  (define (multiply x y) (* x y))
  (accumulate multiply 1 term a next b))

(define (product-iter term a next b)
  (define (multiply x y) (* x y))
  (accumulate-iter multiply 1 term a next b))

(define (identity x) x)

(define (inc x) (+ 1 x))

(define (factorial x)
  (product identity 1 inc x))
