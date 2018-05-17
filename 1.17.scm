#lang sicp

(define (double n) (+ n n))

(define (halve n) (/ n 2))

(define (mul a b)
  (cond ((or (= a 0) (= b 0)) 0)
        ((= b 1) a)
        ((even? b) (mul (double a) (halve b)))
        (else (+ a (mul a (- b 1))))))
