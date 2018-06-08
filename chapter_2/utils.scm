#lang sicp
(#%require racket/provide)

(define (square x) (* x x))

(define (cube x) (* x x x))

;; helper functions
(define (filter predicate list)
  (cond ((null? list) nil)
        ((predicate (car list))
         (cons (car list)
               (filter predicate (cdr list))))
        (else
         (filter predicate (cdr list)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op
                      initial
                      (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low
            (enumerate-interval
             (+ low 1)
             high))))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))


;; test utils

(#%provide flatmap)
(#%provide filter)
(#%provide accumulate)
(#%provide square)
(#%provide cube)
