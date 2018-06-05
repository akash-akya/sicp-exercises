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

(define (sum-square-prime a b)
  (filterd-accm + 0 square a inc b prime?))


;; prime test
(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n)
         n)
        ((divides? test-divisor n)
         test-divisor)
        (else (find-divisor
               n
               (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))
