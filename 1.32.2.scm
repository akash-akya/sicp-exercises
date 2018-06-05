#lang sicp

(define (accumulate combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a)
              (combiner (term a) result))))

  (iter a null-value))



(define (product term a next b)
  (accumulate * 1 term a next b))

(define (sum term a next b)
  (accumulate + 0 term a next b))



(define (factorial n)
  (define (term x) (if (= 0 x) 1 x))
  (product term 0 inc n))

(define (sum-square n)
  (define (square x) (* x x))
  (sum square 0 inc n))
