#lang sicp
(#%require "utils.scm")
(#%require "stream-utils.scm")

;; Exercise 3.60: With power series represented as streams of
;; coefficients as in Exercise 3.59, adding series is implemented by
;; add-streams. Complete the definition of the following procedure for
;; multiplying series:

;; (define (mul-series s1 s2)
;;   (cons-stream ⟨??⟩ (add-streams ⟨??⟩ ⟨??⟩)))

;; You can test your procedure by verifying that sin 2 ⁡ x + cos 2 ⁡ x =
;; 1 , using the series from Exercise 3.59.

(define (stream-mul a b)
  (stream-map * a b))

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


;; test

(define (accumulate-terms stream n)
  (if (= n 0)
      0
      (+ (stream-car stream) (accumulate-terms (stream-cdr stream) (- n 1)))))

(define (test)
  (define sine2+cons2 (add-streams (mul-series sine-series sine-series)
                                    (mul-series cosine-series cosine-series)))
  (test-equal (accumulate-terms sine2+cons2 10) 1))
