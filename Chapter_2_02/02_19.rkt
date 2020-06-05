#lang sicp

(define (count-change amount)
  (cc amount 5))

(define (cc amount coin-values)
  (cond ((= amount 0)
         1)
        ((or (< amount 0)
             (no-more? coin-values))
         0)
        (else
         (+ (cc
             amount
             (except-first-denomination
              coin-values))
            (cc
             (- amount
                (first-denomination
                 coin-values))
             coin-values)))))

(define (length items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))))

(define except-first-denomination cdr)

(define first-denomination car)

(define (no-more? items)
  (null? items))
