#lang sicp

;; Exercise 2.58: Suppose we want to modify the differentiation program
;; so that it works with ordinary mathematical notation, in which + and *
;; are infix rather than prefix operators. Since the differentiation
;; program is defined in terms of abstract data, we can modify it to work
;; with different representations of expressions solely by changing the
;; predicates, selectors, and constructors that define the representation
;; of the algebraic expressions on which the differentiator is to
;; operate.

;; 1. Show how to do this in order to differentiate algebraic expressions
;; presented in infix form, such as (x + (3 * (x + (y + 2)))). To
;; simplify the task, assume that + and * always take two arguments and
;; that expressions are fully parenthesized.

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
        (else (list a1 '+ a2))))


(define (make-product m1 m2)
  (cond ((or (=number? m1 0)
             (=number? m2 0))
         0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2))
         (* m1 m2))
        (else (list m1 '* m2))))

(define (sum? x)
  (and (pair? x) (eq? (cadr x) '+)))

(define (addend s) (car s))

(define (augend s) (caddr s))

(define (product? x)
  (and (pair? x) (eq? (cadr x) '*)))

(define (multiplier p) (car p))

(define (multiplicand p) (caddr p))

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
  (cond ((assert (deriv '(x + 4) 'x) 1))
        ((assert (deriv '(x * 8) 'x) 8))
        ((assert (deriv '(x + (3 * (x + (y + 2)))) 'x) 4))
        (else (display "All cases passed")))
  (newline))
