#lang sicp

(define (double func)
  (lambda (x)
    (func (func x))))


(define (test)
  (= ((double inc) 1) 3))


(((double (double double)) inc) 5)
