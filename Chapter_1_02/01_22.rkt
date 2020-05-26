#lang sicp

(define (square x)
  (* x x))
;Check prime
(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n)
         n)
        ((divides? test-divisor n)
         test-divisor)
        (else (find-divisor
               n
               (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

;Do the timing
(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime)
                       start-time))))
(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (search-for-primes n times)
  (cond ((= times 0) (newline))
        ((prime? n)
         (timed-prime-test n)
         (search-for-primes (+ n 1) (- times 1)))
        (else (search-for-primes (+ n 1) times))))

;(search-for-primes 100000000000 3)
;100000000003 *** 8422
;100000000019 *** 8379
;100000000057 *** 8316
;
;(search-for-primes 1000000000000 3)
;1000000000039 *** 26255
;1000000000061 *** 26232
;1000000000063 *** 26761
;
;(search-for-primes 10000000000000 3)
;10000000000037 *** 82653
;10000000000051 *** 82618
;10000000000099 *** 83128
