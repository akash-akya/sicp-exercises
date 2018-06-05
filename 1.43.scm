#lang sicp

(define (square x) (* x x))

(define (compose f g)
  (lambda (x)
    (f (g x))))

(define (repeated f n)
  (if (= n 0)
      identity  ; apply the f 'zero' times. ie return the argument as it is
      (compose f
               (repeated f (- n 1)))))


((repeated square 2) 5)
