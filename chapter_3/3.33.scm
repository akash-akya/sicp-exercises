#lang sicp
(#%require "utils.scm")
(#%require "constraints_utils.scm")

;; Exercise 3.33: Using primitive multiplier, adder, and constant
;; constraints, define a procedure averager that takes three
;; connectors a, b, and c as inputs and establishes the constraint
;; that the value of c is the average of the values of a and b.


(define A (make-connector))

(define B (make-connector))

(define C (make-connector))

(define (average A B C)
  (let ((by_2 (make-connector))
        (temp (make-connector)))
    (adder A B temp)
    (multiplier temp by_2 C)
    (constant 0.5 by_2)
    'ok))

(average A B C)

;; test
(define (test-average)
  (forget-value! A 'user)
  (forget-value! B 'user)
  (forget-value! C 'user)

  ;; (probe "A" A)
  ;; (probe "B" B)
  ;; (probe "C" C)

  (set-value! A 8 'user)
  (test-equal (get-value A) 8)

  (set-value! B 10 'user)
  (test-equal (get-value B) 10)

  (test-equal (get-value C) 9.0)

  (forget-value! B 'user)
  (set-value! C 15 'user)

  ;; (C*2)-A  => (25*2)-8
  (test-equal (get-value B) 22.0))

(test-average)
