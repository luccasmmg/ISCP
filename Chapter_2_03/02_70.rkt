#lang sicp

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set)))
         (cons x set))
        (else
         (cons (car set)
               (adjoin-set x (cdr set))))))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))
(define (leaf? object)
  (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))
(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set
         (make-leaf (car pair)    ; symbol
                    (cadr pair))  ; frequency
         (make-leaf-set (cdr pairs))))))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left)
                (symbols right))
        (+ (weight left) (weight right))))

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

(define (successive-merge leaf-set)
  (if (null? (cddr leaf-set))
      (make-code-tree (car leaf-set) (cadr leaf-set))
      (successive-merge
       (adjoin-set
        (make-code-tree (car leaf-set)
                        (cadr leaf-set))
        (cddr leaf-set)))))

(define (generate-huffman-tree pairs)
  (successive-merge
   (make-leaf-set pairs)))

(define alphabet '((boom 1) (Wah 1) (Get 2) (job 2) (a 2) (Sha 3) (yip 9) (na 16)))

(define message
  '(Get a job
Sha na na na na na na na na

Get a job
Sha na na na na na na na na

Wah yip yip yip yip
yip yip yip yip yip
Sha boom))
