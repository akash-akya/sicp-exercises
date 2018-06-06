#lang sicp

;; Exercise 2.1: Define a better version of make-rat that handles both positive and
;; negative arguments. Make-rat should normalize the sign so that if the rational number
;; is positive, both the numerator and denominator are positive, and if the rational
;; number is negative, only the numerator is negative.


(define (numer x) (car x))
(define (denom x) (cdr x))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(define (sign x)
  (/ x (abs x)))

(define (make-rat n d)
  (let ((g (gcd n d))
        (s (* (sign n) (sign d))))
    (cons (* (/ n g) s)
          (/ d g))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))


;; Test
(define (test-rat a b)
  (equal-rat? (make-rat (car a)
                        (cdr a))
              (make-rat (car b)
                        (cdr b))))

(define (test)
  (if (and (test-rat (cons -6  9) (cons -2 3))
           (test-rat (cons  6 -9) (cons -2 3))
           (test-rat (cons -6 -9) (cons  2 3))
           (test-rat (cons  6  9) (cons  2 3)))
      (display "All tests passed")
      (display "Few tests failed"))
  (newline))
