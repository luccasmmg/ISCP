#lang sicp

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define (leaf? object)
  (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left)
                (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (encode-symbol-iter symbol tree)
  (define (encode-1 bits current-branch)
    (cond ((leaf? current-branch) bits)
          ((memq symbol (symbols (car current-branch))) (encode-1 (append bits (list 0)) (car current-branch)))
          ((memq symbol (symbols (cadr current-branch))) (encode-1 (append bits (list 1)) (cadr current-branch)))
          (else (error "No symbol in tree"))))
  (encode-1 '() tree))

(define (encode-symbol symbol tree)
    (cond ((leaf? tree) '())
          ((memq symbol (symbols (car tree))) (cons 0 (encode-symbol symbol (car tree))))
          ((memq symbol (symbols (cadr tree))) (cons 1 (encode-symbol symbol (cadr tree))))
          (else (error "No symbol in tree"))
          ))

(define (encode message tree)
  (if (null? message)
      '()
      (append
       (encode-symbol (car message)
                      tree)
       (encode (cdr message) tree))))
