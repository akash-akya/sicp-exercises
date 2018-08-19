#lang sicp
(#%require "utils.scm")
(#%require "stream-utils.scm")

;;  Exercise 3.59: In 2.5.3 we saw how to implement a polynomial
;;  arithmetic system representing polynomials as lists of terms. In a
;;  similar way, we can work with power series, such as

;;      e x = 1 + x + 1 2 x 2 + 1 3 ⋅ 2 x 3 + 1 4 ⋅ 3 ⋅ 2 x 4 + … ,
;;      cos ⁡ x = 1 − 1 2 x 2 + 1 4 ⋅ 3 ⋅ 2 x 4 − … , sin ⁡ x = x − 1 3
;;      ⋅ 2 x 3 + 1 5 ⋅ 4 ⋅ 3 ⋅ 2 x 5 − …

;;  represented as infinite streams. We will represent the series a 0
;;  + a 1 x + a 2 x 2 + a 3 x 3 + … as the stream whose elements are
;;  the coefficients a 0 , a 1 , a 2 , a 3 , ….

;;  The integral of the series a 0 + a 1 x + a 2 x 2 + a 3 x 3 + … is
;;  the series c + a 0 x + 1 2 a 1 x 2 + 1 3 a 2 x 3 + 1 4 a 3 x 4 + …
;;  , where c is any constant. Define a procedure integrate-series
;;  that takes as input a stream a 0 , a 1 , a 2 , … representing a
;;  power series and returns the stream a 0 , 1 2 a 1 , 1 3 a 2 , … of
;;  coefficients of the non-constant terms of the integral of the
;;  series. (Since the result has no constant term, it doesn’t
;;  represent a power series; when we use integrate-series, we will
;;  cons on the appropriate constant.)  The function x ↦ e x is its
;;  own derivative. This implies that e x and the integral of e x are
;;  the same series, except for the constant term, which is e 0 = 1
;;  . Accordingly, we can generate the series for e x as

;;     (define exp-series
;;       (cons-stream
;;        1 (integrate-series exp-series)))

;;  Show how to generate the series for sine and cosine, starting from
;;  the facts that the derivative of sine is cosine and the derivative
;;  of cosine is the negative of sine:

;;     (define cosine-series
;;       (cons-stream 1 ⟨??⟩))

;;     (define sine-series
;;       (cons-stream 0 ⟨??⟩))


;; 1. Integral of series

(define (stream-mul a b)
  (stream-map * a b))

(define (integrate-series coeffs)
  (stream-mul (stream-map (lambda (i) (/ 1 i)) integers)
              coeffs))

;; 2.
(define exp-series
  (cons-stream
   1 (integrate-series exp-series)))

(define cosine-series
  (cons-stream 1 (stream-map
                  (lambda (x) (* -1 x))
                  (integrate-series sine-series))))

(define sine-series
  (cons-stream 0 (integrate-series cosine-series)))

;; test
(define (test)
  (test-equal (stream-ref sine-series 3) (/ -1 6))
  (test-equal (stream-ref sine-series 4) 0)
  (test-equal (stream-ref cosine-series 6) (/ -1 720))
  (test-equal (stream-ref cosine-series 7) 0))
