#lang sicp

(define (even? n)
  (= (remainder n 2) 0))

(define (fast-expt a b n)
  (cond ((<= n 1) a)
        ((even? n) (fast-expt (* a b b)
                              b
                              (/ n 2)))
        (else (fast-expt (* a b)
                         b
                         (- n 1)))))
