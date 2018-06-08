#lang sicp

;; Exercise 2.34: Evaluating a polynomial in x at a given value of x can
;; be formulated as an accumulation. We evaluate the polynomial a n x n +
;; a n − 1 x n − 1 + ⋯ + a 1 x + a 0 using a well-known algorithm called
;; Horner’s rule, which structures the computation as
;; ( … ( a n x + a n − 1 ) x + ⋯ + a 1 ) x + a 0 .  In other words, we
;; start with a n , multiply by x , add a n − 1 , multiply by x , and so
;; on, until we reach a 0 .82

;; Fill in the following template to produce a procedure that evaluates a
;; polynomial using Horner’s rule. Assume that the coefficients of the
;; polynomial are arranged in a sequence, from a 0 through a n .

;; (define
;;   (horner-eval x coefficient-sequence)
;;   (accumulate
;;    (lambda (this-coeff higher-terms)
;;      ⟨??⟩)
;;    0
;;    coefficient-sequence))

;; For example, to compute 1 + 3 x + 5 x 3 + x 5 at x = 2 you would evaluate

;; (horner-eval 2 (list 1 3 0 5 0 1))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op
                      initial
                      (cdr sequence)))))

(define
  (horner-eval x coefficient-sequence)
  (accumulate
   (lambda (this-coeff higher-terms)
     (+ this-coeff
        (* x higher-terms)))
   0
   coefficient-sequence))


;; test
(define (test)
  (cond ((= (horner-eval 2 (list 1 3 0 5 0 1)) 79)
         (display "Test passed"))
        (else (display "Test failed")))
  (newline))
