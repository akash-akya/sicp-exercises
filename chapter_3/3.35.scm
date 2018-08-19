#lang sicp
(#%require "utils.scm")
(#%require "constraints_utils.scm")

;; Exercise 3.35: Ben Bitdiddle tells Louis that one way to avoid the
;; trouble in Exercise 3.34 is to define a squarer as a new primitive
;; constraint. Fill in the missing portions in Ben’s outline for a
;; procedure to implement such a constraint:

(define (squarer a b)
  (define (process-new-value)
    (if (has-value? b)
        (if (< (get-value b) 0)
            (error "square less than 0:
                    SQUARER"
                   (get-value b))
            (set-value! a (sqrt (get-value b)) me))
        (if (has-value? a)
            (set-value! b (* (get-value a) (get-value a)) me)
            'ignored)))

  (define (process-forget-value)
    (forget-value! a me)
    (forget-value! b me)
    (process-new-value))

  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          (else (error "Unknown request:
                        ADDER" request))))
  (connect a me)
  (connect b me)
  me)


(define A (make-connector))
(define S (make-connector))
(squarer A S)

;; test
(define (test-squarer)
  (forget-value! A 'user)
  (forget-value! S 'user)

  (set-value! A 8 'user)
  (test-equal (get-value A) 8)
  (test-equal (get-value S) 64)

  (forget-value! A 'user)
  (set-value! S 16 'user)
  (test-equal (get-value A) 5))

(test-squarer)
