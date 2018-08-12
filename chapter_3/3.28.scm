#lang sicp
(#%require "utils.scm")

;; Exercise 3.28: Define an or-gate as a primitive function box. Your
;; or-gate constructor should be similar to and-gate.

(define (make-wire)
  (cons 0 nil))

(define (get-signal wire)
  (car wire))

(define (set-signal! wire value)
  (define (call-actions actions)
    (cond ((null? actions))
          (else
           ((car actions))
           (call-actions (cdr actions)))))
  (set-car! wire value)
  (call-actions (cdr wire)))

(define (add-action! wire action)
  (set-cdr! wire (cons action (cdr wire))))


(define or-gate-delay 0)

;; dummy funtion with delay 'zero'
(define (after-delay delay function)
  (function))

(define (or-gate a1 a2 output)
  (define (or-action-procedure)
    (let ((new-value
           (logical-or (get-signal a1)
                       (get-signal a2))))
      (after-delay
       or-gate-delay
       (lambda ()
         (set-signal! output new-value)))))
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure)
  'ok)

(define (logical-or a b)
  (cond ((= a 1) 1)
        ((= b 1) 1)
        (else 0)))

;; test
(define (test-or-gate)
  (let ((a (make-wire))
        (b (make-wire))
        (output (make-wire)))
    (or-gate a b output)

    ;; 0,0 => 0
    (test-equal (get-signal output) 0)

    ;; 1,0 => 1
    (set-signal! a 1)
    (test-equal (get-signal output) 1)

    ;; 1,1 => 1
    (set-signal! b 1)
    (test-equal (get-signal output) 1)

    ;; 0,0 => 0
    (set-signal! a 0)
    (set-signal! b 0)
    (test-equal (get-signal output) 0)))

(test-or-gate)
