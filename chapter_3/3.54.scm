#lang sicp
(#%require "utils.scm")
(#%require "stream-utils.scm")

;; Exercise 3.54: Define a procedure mul-streams, analogous to
;; add-streams, that produces the elementwise product of its two input
;; streams. Use this together with the stream of integers to complete
;; the following definition of the stream whose n th element (counting
;; from 0) is n + 1 factorial:

;; (define factorials
;;   (cons-stream 1 (mul-streams ⟨??⟩ ⟨??⟩)))

(define ones (cons-stream 1 ones))

(define integers
  (cons-stream 1 (add-streams ones integers)))

(define (mul-streams a b)
  (stream-map * a b))

(define factorials
  (cons-stream 1 (mul-streams factorials integers)))

;; test
(define (test)
  (test-equal (stream-ref factorials 5) 120))

(test)
