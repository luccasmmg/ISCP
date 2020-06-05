#lang sicp

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op
                      initial
                      (cdr sequence)))))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

(define (matrix-*-vector m v)
  (map (lambda(x) (accumulate + 0 (map * x v))) m))

(define matrix (list (list 0 3 5) (list 5 5 2)))
(define matrix-2 (list (list 3 4) (list 3 -2) (list 4 -2)))
(define vector (list (list 3 3 4)))

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (transpose mat)
  (accumulate-n cons nil mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda(x) (map (lambda(y) (dot-product x y)) cols)) m)))
