#lang sicp

(define (filterd-accm combiner null-value term a next b prediate)
  (define (new-result a result)
    (if (prediate a)
        (combiner (term a) result)
        result))

  (define (iter a result)
    (if (> a b)
        result
        (iter (next a)
              (new-result a result))))

  (iter a null-value))

(define (square x) (* x x))

(define (product-relative-primes n)
  (define (rprime? i) (relative-prime? i n))
  (filterd-accm * 1 identity 1 inc (- n 1) rprime?))


;; GCD
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (relative-prime? n i)
  (= (gcd n i) 1))
