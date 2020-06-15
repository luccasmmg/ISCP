#lang racket
;Exponentiation
;(* (* n (** u (- n 1)) (/ (deriv u) (deriv x))))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product
           (multiplier exp)
           (deriv (multiplicand exp) var))
          (make-product
           (deriv (multiplier exp) var)
           (multiplicand exp))))
        ((exponentiation? exp)
         (make-product
          (make-product
           (exponent exp)
           (make-exponentiation
            (base exp)
            (make-sum (exponent exp) ('-1))))
          (deriv (base exp) var)))
        (else (error "unknown expression
                      type: DERIV" exp))))

(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**)))

(define (base p) (cadr p))

(define (exponent p) (caddr p))

(define (variable? x) (symbol? x))

;Anything raised to the power of 0 is 1
;Anything raised to the power 1 is the thing itself
(define (make-exponentiation e1 e2)
  (cond ((or (=number? e1 0)
             (=number? e2 0))
         0)
        ((=number? e2 0) 1)
        ((=number? e2 1) e1)
        (else (list '** e1 e2))))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (same-variable? v1 v2)
  (and (variable? v1)
       (variable? v2)
       (eq? v1 v2)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2))
         (+ a1 a2))
        (else (list '+ a1 a2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0)
             (=number? m2 0))
         0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2))
         (* m1 m2))
        (else (list '* m1 m2))))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define (addend s) (cadr s))

(define (augend s)
   (caddr s))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (multiplier p) (cadr p))

(define (multiplicand p)
   (caddr p))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op
                      initial
                      (cdr sequence)))))