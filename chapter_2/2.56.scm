#lang sicp

;; Exercise 2.56: Show how to extend the basic differentiator to handle
;; more kinds of expressions. For instance, implement the differentiation
;; rule

;;    du^n/dx = n*u^(n-1) du/dx

;; by adding a new clause to the deriv program and defining appropriate
;; procedures exponentiation?, base, exponent, and
;; make-exponentiation. (You may use the symbol ** to denote
;; exponentiation.) Build in the rules that anything raised to the power
;; 0 is 1 and anything raised to the power 1 is the thing itself.


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

(define (augend s) (caddr s))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (multiplier p) (cadr p))

(define (multiplicand p) (caddr p))

;; Exponent
(define (exponentation? exp)
  (and (pair? exp) (eq? (car exp) '**)))

(define (base exp) (cadr exp))

(define (exponent exp) (caddr exp))

(define (make-exponentation base exponent)
  (cond ((=number? exponent 0) 1)
        ((=number? exponent 1) base)
        (else (list '** base exponent))))

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
        ((exponentation? exp)
         (let ((b (base exp))
               (e (exponent exp)))
           (make-product (make-product e (make-exponentation b (- e 1)))
                         (deriv b var))))
        (else (error "unknown expression" exp))))
