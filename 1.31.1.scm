#lang sicp

(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

(define (factorial n)
  (define (identity x) (if (= 0 x) 1 x))
  (product identity 0 inc n))


(define (compute-pi)
  (define (square x) (* x x))

  (define (term i)
    (/ (* i (+ 2 i))
       (square (+ 1 i))))

  (define (next i)
    (+ i 2))

  (* 4.0 (product term 2 next 1000)))
