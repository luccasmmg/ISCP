#lang sicp

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low
            (enumerate-interval
             (+ low 1)
             high))))

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate
                       (cdr sequence))))
        (else (filter predicate
                      (cdr sequence)))))
(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op
                      initial
                      (cdr sequence)))))

(define (unique-triples n)
  (accumulate append '() (flatmap
   (lambda (i)
     (map (lambda (j)
            (map (lambda(k)
                   (list i j k))
                 (enumerate-interval 1 (- j 1))))
          (enumerate-interval 1 (- i 1))))
   (enumerate-interval 1 n))) )

(define (triple-sum number)
  (lambda (x) (= (+ (car x) (cadr x) (cadr (cdr x))) number) ))

(define (exercise_02_41 n s)
  (map make-triple-sum
       (filter
        (triple-sum s)
        (unique-triples n))))

(define (make-triple-sum triple)
  (list (car triple)
        (cadr triple)
        (cadr (cdr triple))
        (+ (car triple) (cadr triple) (cadr (cdr triple))))
        )
