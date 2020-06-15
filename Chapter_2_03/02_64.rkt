#lang sicp

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (list->tree elements)
  (car (partial-tree
        elements (length elements))))

;First you cut the tree more or less in half, in this case the left
;will be 2 and the right 3, the left result will be a recursive call to partial tree
;but now with just the 2 first elements(1 and 3) the first node of the left tree will be
;the car of this result(in this case 1), and the right elements will be the cdr(in this case 3)
;after populating the left-tree, we populate the right tree, the size will be 3 in this case
;this-entry will be the first element in the non-left elements, and the right-result will be a
;a partial tree with all the non-left elements (7, 9, 11)
(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size
             (quotient (- n 1) 2)))
        (let ((left-result
               (partial-tree
                elts left-size)))
          (let ((left-tree
                 (car left-result))
                (non-left-elts
                 (cdr left-result))
                (right-size
                 (- n (+ left-size 1))))
            (let ((this-entry
                   (car non-left-elts))
                  (right-result
                   (partial-tree
                    (cdr non-left-elts)
                    right-size)))
              (let ((right-tree
                     (car right-result))
                    (remaining-elts
                     (cdr right-result)))
                (cons (make-tree this-entry
                                 left-tree
                                 right-tree)
                      remaining-elts))))))))
