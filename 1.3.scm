#lang sicp

(define (large-two a b c)
  (if (or (> a b) (> a c))
      (if (> b c)
          (cons a b)
          (cons a c))
      (cons b c)))

(define (square a) (* a a))

(define (sum-of-square a b c)
  (define pair (large-two a b c))
  (+ (square (car pair))
     (square (cdr pair))))
