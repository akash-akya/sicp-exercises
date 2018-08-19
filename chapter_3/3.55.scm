#lang sicp
(#%require "utils.scm")
(#%require "stream-utils.scm")

;; Exercise 3.55: Define a procedure partial-sums that takes as
;; argument a stream S and returns the stream whose elements are S 0 ,
;; S 0 + S 1 , S 0 + S 1 + S 2 , … . For example, (partial-sums
;; integers) should be the stream 1, 3, 6, 10, 15, ….

(define ones (cons-stream 1 ones))

(define integers
  (cons-stream 1 (add-streams ones integers)))

;; Initial Version
(define (partial-sums s)
  (define sums
    (cons-stream (stream-car s)
                 (add-streams (stream-cdr s) sums)))
  sums)


;; After substitution
(define (partial-sums-updated s)
  (cons-stream (stream-car s)
               (add-streams (stream-cdr s)
                            (partial-sums-updated s))))

;; test
(define (test)
  (test-equal (stream-ref (partial-sums integers) 4) 15)
  (test-equal (stream-ref (partial-sums-updated integers) 4) 15))
