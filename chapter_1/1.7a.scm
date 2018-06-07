#lang sicp

(define (average a b)
  (/ (+ a b) 2))

(define (square-root x)
  (define (good-enough? prev current)
    (< (abs (- current prev))
       0.0000001))

  (define (nw-square-root x prev guess)
    (if (good-enough? prev guess)
        guess
        (nw-square-root x guess (average guess (/ x guess)))))
  (nw-square-root x 0 1.0))

(square-root 0.0000001)

