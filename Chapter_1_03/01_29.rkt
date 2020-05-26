#lang sicp

(define (cube x)
  (* x x x))

(define (inc n) (+ n 1))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (simpson-rule f a b n)
  (define subintervals (/ (- b a) n))
  (define (get-k k)
    (* subintervals k))
  (define (calculate k)
    (f (get-k k)))
  (define (get-factor k)
    (cond ((or (= k 0.0) (= k n) ) (* 1.0 (calculate k)))
          ((odd? k) (* 4.0 (calculate k)))
          (else (* 2.0 (calculate k)))
          ))
  (* (/ subintervals 3)( sum get-factor a inc n )))
