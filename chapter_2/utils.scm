#lang sicp
(#%require racket/base)

(define (square x) (* x x))

(define (cube x) (* x x x))

;; helper functions
(define (filter predicate l)
  (cond ((null? l) nil)
        ((predicate (car l))
         (cons (car l)
               (filter predicate (cdr l))))
        (else
         (filter predicate (cdr l)))))

(define (accumulate op initial l)
  (if (null? l)
      initial
      (op (car l)
          (accumulate op
                      initial
                      (cdr l)))))

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
  (display "Passed..."))

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
(#%provide enumerate-interval)
