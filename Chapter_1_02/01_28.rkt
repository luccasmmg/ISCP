#lang sicp

(define (square x)
  (* x x))

(define (miller-rabin-expmod base exp m)
  (define (check x)
    (if (and (not (= x 1))
             (not (= x (- m 1)))
             (= (remainder (square x) m) 1))
        0
        x))
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder
          (square (check (miller-rabin-expmod base (/ exp 2) m)))
          m))
        (else
         (remainder
          (* base (miller-rabin-expmod base (- exp 1) m))
          m))))

(define (fermat-test n a)
  (= (miller-rabin-expmod a n n) a)
  )

(define (carmichael-number? n)
  (fast-prime? n (- n 1)))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n times)
         (fast-prime? n (- times 1)))
        (else false)))
