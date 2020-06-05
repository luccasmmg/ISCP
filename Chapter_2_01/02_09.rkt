#lang sicp

(define (make-interval a b) (cons a b))

(define (lower-bound x)
  (min (car x) (cdr x)))

(define (upper-bound x)
  (max (car x) (cdr x)))

(define (add-interval x y)
  (make-interval (+ (lower-bound x)
                    (lower-bound y))
                 (+ (upper-bound x)
                    (upper-bound y))))

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (lower-bound y))
                 (- (upper-bound x) (upper-bound y))
                 ))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x)
               (lower-bound y)))
        (p2 (* (lower-bound x)
               (upper-bound y)))
        (p3 (* (upper-bound x)
               (lower-bound y)))
        (p4 (* (upper-bound x)
               (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (if (= 0 ( - (upper-bound y) (lower-bound y) ))
      (error "Y interval spans 0")
  (mul-interval x
                (make-interval
                 (/ 1.0 (upper-bound y))
                 (/ 1.0 (lower-bound y))))))

(define (width-interval x)
  (abs ( / (- (upper-bound x) (lower-bound x)) 2) ))

(define first (make-interval 10 20))
(define second (make-interval 10 30))
(define third (make-interval 10 60))
(define fourth (make-interval 5 10))
(define fifth (make-interval 5 5))

(define first+second (add-interval first second)) ;On theory 15
(define first-second (sub-interval first second)) ;On theory 5
(define first*second (mul-interval first second))
(define first/second (div-interval first second))
