#lang sicp

(define (no-more? coin-values)
  (null? coin-values))

(define (first-denomination coin-values)
  (car coin-values))

(define (except-first-denomination coin-values)
  (cdr coin-values))

(define (cc amount coin-values)
  (cond ((= amount 0)
         1)
        ((or (< amount 0)
             (no-more? coin-values))
         0)
        (else
         (+ (cc
             amount
             (except-first-denomination
              coin-values))
            (cc
             (- amount
                (first-denomination
                 coin-values))
             coin-values)))))


(define us-coins
  (list  5 1 50 10 25))

(define uk-coins
  (list 100 50 20 10 5 2 1 0.5))


(define (test)
  (cond ((= (cc 30 (list 5 1 50 10 25))
            (cc 30 (list 50 25 10 5 1 )))
         (display "Does not depend on order"))
        (else (display "Depends on order")))
  (newline))
