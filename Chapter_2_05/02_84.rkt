#lang sicp

;;Given that the system wasnt working correctly, for instance, i couldnt test
;;the results of exercise 77, i decided to copy this resolution of exercise 79
;;that i found on  the internet. So that i can solve the next exercises and test
;;the results
;;
;; ======================================================================
;; Answers:
;; 1 -> The apply generic procedure will first try to find if there is a procedure
;; with the type 'complex 'complex in the coercion table, in this case it will find,
;; after that it will apply generic again, but the
;;
;; ======================================================================


;; ======================================================================
;;
;; To test the exercises I need an implementation of put and get.
;; These are taken directly from section 3.3.3 of the book
;; http://mitpress.mit.edu/sicp/full-text/book/book-Z-H-22.html#%_sec_3.3.3
;;
;; ======================================================================
(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (cdr record)
                  false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1
                                  (cons key-2 value))
                            (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            ((eq? m 'table) local-table)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

(define coercion-table (make-table))
(define get-coercion (operation-table 'lookup-proc))
(define put-coercion (operation-table 'insert-proc!))

(define (square x) (* x x))

(define (attach-tag type-tag contents)
  (if (eq? type-tag 'scheme-number)
      contents
      (cons type-tag contents)))

(define (type-tag datum)
  (cond ((pair? datum) (car datum))
        ((number? datum) 'scheme-number)
        (else (error "Bad tagged datum -- TYPE-TAG" datum))))

(define (contents datum)
  (cond ((pair? datum) (cdr datum))
        ((number? datum) cdr datum)
        (else (error "Bad tagged datum -- CONTENTS" datum))))

(define (get-highest-level args)
  (define (iter args-left highest)
    (cond ((null? args-left) highest)
          ((> (level? (car args-left)) highest) (iter (cdr args-left) (level? (car args-left))))
          (else (iter (cdr args-left) highest))))
  (iter args 0)
  )

(define (compose f g)
  (lambda (x) (f (g x))))
(define (repeat f n)
  (if (< n 1)
      (lambda (x) x)
      (compose f (repeat f (- n 1)))))
(define (raise-to-level level)
  (lambda (x)
    (let ((steps-to-take (- level (level? x))))
      ((repeat raise steps-to-take) x)
      ))
    )

(define (apply-generic op . args)
    (let ((type-tags (map type-tag args)))
      (let ((proc (get op type-tags)))
        (if proc
            (apply proc (map contents args))
            (if (> (length args) 1)
                (let (( highest-level (get-highest-level args) ))
                  (let (( coerced (map (raise-to-level highest-level) args)))
                    (let (( proc (get op (map type-tag coerced))))
                      (if proc
                          (apply proc (map contents coerced))
                          (error "No method for these types"))
                      )
                    )
                  )
                (error "No method for these types"
                       (list op type-tags)))
            ))))

(define (coerce-list set base)
  (if (null? set)
      '()
      (let (( coercion (get-coercion (type-tag (car set)) (type-tag base)) ))
        (if coercion
            (cons (coercion (car set)) (coerce-list (cdr set) base))
            (cons (car set) (coerce-list (cdr set) base))
            )
        )))

(define (get-coercions args)
  (define (iter to-coerce bases)
    (if (null? bases)
        (if (not (eq? args to-coerce))
            to-coerce
            (error "Failed in coercing"))
        (iter (coerce-list to-coerce (car bases)) (cdr bases))))
  (iter (coerce-list args (car args)) args))
;; ======================================================================
;;
;; The scheme number package
;;
;; ======================================================================
(define (install-scheme-number-package)
  (define (tag x)
    (attach-tag 'scheme-number x))
  (put 'add  '(scheme-number scheme-number) (lambda (x y) (tag (+ x y))))
  (put 'sub  '(scheme-number scheme-number) (lambda (x y) (tag (- x y))))
  (put 'mul  '(scheme-number scheme-number) (lambda (x y) (tag (* x y))))
  (put 'div  '(scheme-number scheme-number) (lambda (x y) (tag (/ x y))))
  (put 'exp '(scheme-number scheme-number) (lambda (x y) (tag (expt x y))))
  (put 'equ? '(scheme-number scheme-number) =)
  (put 'zero? '(scheme-number) zero?)
  (put 'level? '(scheme-number) (lambda (x) 0))
  (put 'raise '(scheme-number) (lambda (x) (make-rational x 1)))
  (put 'make 'scheme-number (lambda (x) (tag x)))
  'done)

;; ======================================================================
;;
;; The rectangular number package
;;
;; ======================================================================
(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

;; ======================================================================
;;
;; The polar number package
;;
;; ======================================================================
(define (install-polar-package)
  ;; internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)


;; ======================================================================
;;
;; The rational number package
;;
;; ======================================================================
(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d) (let ((g (gcd n d)))
                           (cons (/ n g) (/ d g))))
  (define (add-rat x y) (make-rat (+ (* (numer x) (denom y))
                                     (* (numer y) (denom x)))
                                  (* (denom x) (denom y))))
  (define (add-rat-3 x y z) (add-rat (add-rat x y) z))
  (define (sub-rat x y) (make-rat (- (* (numer x) (denom y))
                                     (* (numer y) (denom x)))
                                  (* (denom x) (denom y))))
  (define (mul-rat x y) (make-rat (* (numer x) (numer y))
                                  (* (denom x) (denom y))))
  (define (div-rat x y) (make-rat (* (numer x) (denom y))
                                  (* (denom x) (numer y))))
  (define (equ-rat x y) (and (equ? (numer x) (numer y))
                             (equ? (denom x) (denom y))))
  (define (zero-rat? x) (and (zero? (numer x)) (not (zero? denom x))))

  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)  (lambda (x y) (tag (add-rat x y))))
  (put 'add-3'(rational rational rational)  (lambda (x y z) (tag (add-rat-3 x y z))))
  (put 'sub '(rational rational)  (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)  (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)  (lambda (x y) (tag (div-rat x y))))
  (put 'level? '(rational) (lambda (x) 1))
  (put 'raise '(rational) (lambda (x) (make-complex-from-real-imag (/ (* 1.0 (numer x)) (* 1.0 (denom x))) 0)))
  (put 'equ? '(rational rational) equ-rat)
  (put 'zero? '(rational) zero-rat?)
  (put 'make 'rational (lambda (n d) (tag (make-rat n d))))
  'done)

;; ======================================================================
;;
;; The complex number package
;;
;; ======================================================================
(define (install-complex-package)
  ;; imported procedures from rectangular and polar packages
  (define (make-from-real-imag x y) ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a) ((get 'make-from-mag-ang 'polar) r a))
  ;; internal procedures
  (define (add-complex z1 z2) (make-from-real-imag (+ (real-part z1) (real-part z2))
                                                   (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2) (make-from-real-imag (- (real-part z1) (real-part z2))
                                                   (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2) (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                                                 (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2) (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                                                 (- (angle z1) (angle z2))))
  (define (equ-complex z1 z2) (and (equ? (magnitude z1) (magnitude z2))
                                   (equ? (angle z1) (angle z2))))
  (define (zero-complex? z) (zero? (magnitude z)))
  ;; interface to rest of the system
  (define (tag z) (attach-tag 'complex z))
  (put 'add  '(complex complex) (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'add-3  '(complex complex complex) (lambda (z1 z2 z3) (tag (add-complex (add-complex z1 z2) z3))))
  (put 'sub  '(complex complex) (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul  '(complex complex) (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div  '(complex complex) (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'equ? '(complex complex) equ-complex)
  (put 'make-from-real-imag 'complex (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex (lambda (r a) (tag (make-from-mag-ang r a))))
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle     '(complex) angle)
  (put 'level? '(complex) (lambda (x) 2))
  'done)

;; ======================================================================
;;
;; Generic procedures
;;
;; ======================================================================

; Constructors
(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))
(define (make-rational numer denom)
  ((get 'make 'rational) numer denom))
(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

(define (add x y) (apply-generic 'add x y))
(define (add-3 x y z) (apply-generic 'add-3 x y z))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle     z) (apply-generic 'angle     z))
(define (raise     z) (apply-generic 'raise     z))
(define (level?     z) (apply-generic 'level?     z))
(define (equ? x y) (apply-generic 'equ? x y))
(define (zero? x) (apply-generic 'zero? x))
(define (exp x y) (apply-generic 'exp x y))


;; ======================================================================
;;
;; Package installation
;;
;; ======================================================================
(define (install-number-packages)
  (install-scheme-number-package)
  (install-polar-package)
  (install-rectangular-package)
  (install-rational-package)
  (install-complex-package))

(install-number-packages)
