#lang sicp

(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))
(define (set-front-ptr! queue item)
  (set-car! queue item))
(define (set-rear-ptr! queue item)
  (set-cdr! queue item))

(define (empty-queue? queue)
  (null? (front-ptr queue)))
(define (make-queue) (cons '() '()))
(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with an empty queue"
             queue)
      (car (front-ptr queue))))
(define (insert-rear-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (else (set-cdr! (rear-ptr queue)
                          new-pair)
                (set-rear-ptr! queue new-pair)
                queue))))
(define (delete-front-queue! queue)
  (cond ((empty-queue? queue)
         (error "DELETE! caled with empty queue"
                queue))
        (else (set-front-ptr!
               queue
               (cdr (front-ptr queue)))
              queue)))
(define (delete-rear-queue! queue)
  (cond ((empty-queue? queue)
         (error "DELETE! caled with empty queue"
                queue))
        (else (set-rear-ptr!
               queue
               (cddr (rear-ptr queue)))
              queue)))
(define (print-queue queue)
  (for-each display (front-ptr queue)))
