#lang sicp

(define (product term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a)
              (* (term a) result))))
  (iter a 1))

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
