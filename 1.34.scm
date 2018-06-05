#lang sicp

(define (f g) (g 2))


(f (lambda (x) (* x x)))

;; (f f)
