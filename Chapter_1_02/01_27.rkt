#lang racket

(define (square x)
  (* x x))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder
          (square (expmod base (/ exp 2) m))
          m))
        (else
         (remainder
          (* base (expmod base (- exp 1) m))
          m))))

(define (fermat-test n a)
  (= (expmod a n n) a)
  )

(define (carmichael-number? n)
  (fast-prime? n (- n 1)))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n times)
         (fast-prime? n (- times 1)))
        (else false)))
