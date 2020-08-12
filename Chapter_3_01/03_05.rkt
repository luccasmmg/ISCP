#lang sicp

(define (square x) (* x x))

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0.0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1.0)
                 (+ trials-passed 1.0)))
          (else
           (iter (- trials-remaining 1.0)
                 trials-passed))))
  (iter trials 0.0))

(define (pi-test X Y)
  (lambda() (<=
             (+
              (square (- (random-in-range 0.0 10.0) 5.0) )
              (square (- (random-in-range 0.0 10.0) 5.0) ))
             (square 5.0)
             )))

(define (estimate-integral P X Y trials)
  (let (( area-square (* (- (cadr X) (car X)) (- (cadr Y) (car Y))) ))
    (* 4.0 (monte-carlo trials (P X Y)))
    ))
