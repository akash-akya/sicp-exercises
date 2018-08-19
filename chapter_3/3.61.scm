#lang sicp
(#%require "utils.scm")
(#%require "stream-utils.scm")

;; Exercise 3.61: Let S be a power series (Exercise 3.59) whose
;; constant term is 1. Suppose we want to find the power series 1 / S
;; , that is, the series X such that S X = 1 . Write S = 1 + S R where
;; S R is the part of S after the constant term. Then we can solve for

;; X as follows:

;; S ⋅ X = 1 , ( 1 + S R ) ⋅ X = 1 , X + S R ⋅ X = 1 , X = 1 − S R ⋅ X
;; .  In other words, X is the power series whose constant term is 1
;; and whose higher-order terms are given by the negative of S R times
;; X . Use this idea to write a procedure invert-unit-series that
;; computes 1 / S for a power series S with constant term 1. You will
;; need to use mul-series from Exercise 3.60.


;; Form 3.60.scm
(define (stream-mul a b)
  (stream-map * a b))

(define ones (cons-stream 1 ones))

(define integers
  (cons-stream 1 (add-streams ones integers)))

(define (integrate-series coeffs)
  (stream-mul (stream-map (lambda (i) (/ 1 i)) integers)
              coeffs))

(define exp-series
  (cons-stream
   1 (integrate-series exp-series)))

(define cosine-series
  (cons-stream 1 (stream-map
                  (lambda (x) (* -1 x))
                  (integrate-series sine-series))))

(define sine-series
  (cons-stream 0 (integrate-series cosine-series)))


(define (scale-stream stream factor)
  (stream-map
   (lambda (x) (* x factor))
   stream))

(define (mul-series s1 s2)
  (cons-stream (* (stream-car s1) (stream-car s2))
               (add-streams (scale-stream (stream-cdr s1) (stream-car s2))
                            (mul-series s1 (stream-cdr s2)))))

(define (display-stream-n stream n)
  (if (= n 0)
      (newline)
      (begin
        (display (stream-car stream)) (display "  ")
        (display-stream-n (stream-cdr stream) (- n 1)))))


(define (invert-unit-series stream)
  (cons-stream (* 1 (stream-car stream))
               (mul-series stream (invert-unit-series stream))))

;; test
