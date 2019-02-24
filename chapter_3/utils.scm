#lang sicp
(#%require racket/base)
;; (#%require (only racket/base print-as-expression print-mpair-curly-braces))
(print-as-expression #f)
(print-mpair-curly-braces #f)

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
  (display "✔ ")
  (display lhs) (display " = ") (display rhs))

(define (failure lhs rhs lhs-result rhs-result)
  (display "✖ ")
  (display lhs) (display " = ") (display rhs)
  (newline)
  (display "    Left  : ") (display lhs-result)
  (newline)
  (display "    Right : ") (display rhs-result))

;; test utils
(define-syntax-rule (test-equal lhs rhs)
  (let ((qlhs (quote lhs))
        (qrhs (quote rhs))
        (lhs-result lhs)
        (rhs-result rhs))
    (cond [(equal? lhs-result rhs-result) (success qlhs qrhs)]
          [else (failure qlhs qrhs lhs-result rhs-result)])
    (newline)))


(#%provide flatmap)
(#%provide filter)
(#%provide accumulate)
(#%provide square)
(#%provide cube)
(#%provide test-equal)
