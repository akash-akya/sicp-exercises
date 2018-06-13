#lang sicp

;; Exercise 2.57: Extend the differentiation program to handle sums and
;; products of arbitrary numbers of (two or more) terms. Then the last
;; example above could be expressed as

;;     (deriv '(* x y (+ x 3)) 'x)

;; Try to do this by changing only the representation for sums and
;; products, without changing the deriv procedure at all. For example,
;; the addend of a sum would be the first term, and the augend would be
;; the sum of the rest of the terms.


(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1)
       (variable? v2)
       (eq? v1 v2)))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2))
         (+ a1 a2))
        (else (list '+ a1 a2))))


(define (make-product m1 m2)
  (cond ((or (=number? m1 0)
             (=number? m2 0))
         0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2))
         (* m1 m2))
        (else (list '* m1 m2))))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define (addend s) (cadr s))

(define (augend s)
  (let ((num (cddr s)))
    (if (null? num)
        0
        (cons '+ num))))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (multiplier p) (cadr p))

(define (multiplicand p)
  (let ((num (cddr p)))
    (if (null? num)
        1
        (cons '* num))))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum (make-product (deriv (multiplicand exp) var)
                                 (multiplier exp))
                   (make-product (multiplicand exp)
                                 (deriv (multiplier exp) var))))
        (else (error "unknown expression" exp))))


;; test
(define (assert exp result)
  (cond ((not (equal? exp result))
         (display "Failed!")
         (display exp)
         #t)
        (else #f)))

(define (test)
  (cond ((assert (deriv '(+ x 4 (* 3 x)) 'x) 4))
        ((assert (deriv '(+ x 4 x y) 'x)     2))
        ((assert (deriv '(* x 9 2) 'x)       '(* 9 2)))
        (else (display "All cases passed")))
  (newline))
