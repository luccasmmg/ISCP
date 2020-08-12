#lang sicp

(define balance 100)

(define (withdraw amount)
  (if (>= balance amount)
      (begin (set! balance (- balance amount))
             balance)
      "Insufficient funds!"))

(define (make-withdraw initial-amount)
  (let (( balance initial-amount ))
    (lambda (amount)
      (if (>= balance amount)
          (begin (set! balance
                       (- balance amount))
                 balance)
          "Insuficient funds"))))

;; (define (make-withdraw balance)
;;   (lambda (amount)
;;     (if (>= balance amount)
;;         (begin (set! balance
;;                      (- balance amount))
;;                balance)
;;         "Insufficient funds")))

(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance
                     (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknow request:
MAKE-ACCOUNT" m))))
  dispatch)
