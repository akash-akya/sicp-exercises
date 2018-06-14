#lang sicp
(#%require racket/provide)
(#%require racket/base)

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


(define (success lhs rhs)
  (display "Success"))

(define (failure lhs rhs)
  (display "Failed!  ")
  (display lhs)
  (display "  =  ")
  (display rhs))

;; test utils
(define-syntax-rule (test-equal lhs rhs)
  (let ((qlhs (quote lhs))
        (qrhs (quote rhs)))
    (cond [(equal? lhs rhs) (success qlhs qrhs)]
          [else (failure qlhs qrhs)])
    (newline)))


(#%provide flatmap)
(#%provide filter)
(#%provide accumulate)
(#%provide square)
(#%provide cube)
(#%provide test-equal)
