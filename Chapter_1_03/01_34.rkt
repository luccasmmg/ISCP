#lang sicp
(define (square x)
  (* x x))

(define (f g) (g 2))

;(f f)
;Vai tentar rodar (g 2) vai virar (f 2)
;(f 2) vai tentar rodar (g 2) vai virar (2 2)
