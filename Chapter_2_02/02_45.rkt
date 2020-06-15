#lang sicp
(#%require sicp-pict)

(define (split x y)
  (define (operation painter n)
    (if (= n 0)
        painter
        (let ((smaller (operation painter (- n 1))))
          (x painter
             (y smaller smaller)))))
  (lambda (to_paint number_of_times) (operation to_paint number_of_times)))
