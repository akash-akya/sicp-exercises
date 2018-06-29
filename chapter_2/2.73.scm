#lang sicp
(#%require "utils.scm")
(#%require "dummy-env-helper.scm")

;; Exercise 2.73: 2.3.2 described a program that performs symbolic differentiation:

;; (define (deriv exp var)
;;   (cond ((number? exp) 0)
;;         ((variable? exp)
;;          (if (same-variable? exp var) 1 0))
;;         ((sum? exp)
;;          (make-sum (deriv (addend exp) var)
;;                    (deriv (augend exp) var)))
;;         ((product? exp)
;;          (make-sum
;;            (make-product
;;             (multiplier exp)
;;             (deriv (multiplicand exp) var))
;;            (make-product
;;             (deriv (multiplier exp) var)
;;             (multiplicand exp))))
;;         ⟨more rules can be added here⟩
;;         (else (error "unknown expression type:
;;                       DERIV" exp))))

;; We can regard this program as performing a dispatch on the type of
;; the expression to be differentiated. In this situation the “type
;; tag” of the datum is the algebraic operator symbol (such as +) and
;; the operation being performed is deriv. We can transform this
;; program into data-directed style by rewriting the basic derivative
;; procedure as

;; (define (deriv exp var)
;;    (cond ((number? exp) 0)
;;          ((variable? exp)
;;            (if (same-variable? exp var)
;;                1
;;                0))
;;          (else ((get 'deriv (operator exp))
;;                 (operands exp)
;;                 var))))

;; (define (operator exp) (car exp))
;; (define (operands exp) (cdr exp))

;; 1.  Explain what was done above. Why can’t we assimilate the
;;     predicates number? and variable? into the data-directed
;;     dispatch?

;; 2.  Write the procedures for derivatives of sums and
;;     products, and the auxiliary code required to install them in
;;     the table used by the program above.

;; 3.  Choose any additional
;;     differentiation rule that you like, such as the one for
;;     exponents (Exercise 2.56), and install it in this data-directed
;;     system.

;; 4.  In this simple algebraic manipulator the type of an
;;     expression is the algebraic operator that binds it
;;     together. Suppose, however, we indexed the procedures in the
;;     opposite way, so that the dispatch line in deriv looked like

;;     ((get (operator exp) 'deriv)
;;      (operands exp) var)

;;     What corresponding changes to the derivative system are required?


(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum:
              TYPE-TAG" datum)))

(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum:
              CONTENTS" datum)))

;; 2.

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1)
       (variable? v2)
       (eq? v1 v2)))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var)
             1
             0))
        (else ((get 'deriv (operator exp))
               (operands exp)
               var))))

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

(define (lhs s) (car s))
(define (rhs s) (cadr s))


(define (install-addition-package)
  ;; interface to the rest of the system
  (define (tag x) (attach-tag '+ x))

  (define (make-sum a1 a2)
    (cond ((=number? a1 0) a2)
          ((=number? a2 0) a1)
          ((and (number? a1) (number? a2))
           (+ a1 a2))
          (else (tag (list a1 a2)))))

  (put '+ nil make-sum)

  (put 'deriv '+
       (lambda (args var)
         (make-sum (deriv (lhs args) var)
                   (deriv (rhs args) var))))
  'done)

(define (make-sum a b)
  ((get '+ nil) a b))

(define (install-product-package)
  (define (make-product m1 m2)
    (cond ((or (=number? m1 0)
               (=number? m2 0))
           0)
          ((=number? m1 1) m2)
          ((=number? m2 1) m1)
          ((and (number? m1) (number? m2))
           (* m1 m2))
          (else (tag (list m1 m2)))))

  ;; interface to the rest of the system
  (define (tag x) (attach-tag '* x))

  (put '* nil make-product)

  (put 'deriv '*
       (lambda (args var)
         (make-sum (make-product (deriv (lhs args) var)
                                 (rhs args))
                   (make-product (lhs args)
                                 (deriv (rhs args) var)))))
  'done)

(define (make-product a b)
  ((get '* nil) a b))



;; test
(install-addition-package)
(install-product-package)

(test-equal (deriv '(+ x 3) 'x) 1)
(test-equal (deriv '(* x y) 'x) 'y)
(test-equal (deriv '(* (* x y) (+ x 3)) 'x) '(+ (* y (+ x 3)) (* x y)))


;; 3.

(define (install-exponentation-package)
  (define (make-exponentation base exponent)
    (cond ((=number? exponent 0) 1)
          ((=number? exponent 1) base)
          (else (tag (list base exponent)))))

  ;; interface to the rest of the system
  (define (tag x) (attach-tag '** x))
  (put 'deriv '**
       (lambda (args var)
         (let ((b (lhs args))
               (e (rhs args)))
           (make-product (make-product e (make-exponentation b (- e 1)))
                         (deriv b var)))))
  'done)

(install-exponentation-package)
(test-equal (deriv '(** x 3) 'x) '(* 3 (** x 2)))


;; 4.
;; just swap the order of 'deriv and operator in put & get function calls
