#lang sicp
(#%require racket/base)

(define (square x) (* x x))

(define (cube x) (* x x x))

;; helper functions
(define (filter predicate list)
  (cond ((null? list) '())
        ((predicate (mcar list))
         (mcons (mcar list)
               (filter predicate (mcdr list))))
        (else
         (filter predicate (mcdr list)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (mcar sequence)
          (accumulate op
                      initial
                      (mcdr sequence)))))


(define (dedup list)
  (define (exists? el list)
    (cond ((null? list) #f)
        ((equal? el (mcar list)) #t)
        (else
         (exists? el (mcdr list)))))
  (define (dedup-iter list acc)
    (cond ((null? list) acc)
          ((exists? (mcar list) acc)
           (dedup-iter (mcdr list) acc))
          (else
           (dedup-iter (mcdr list)
                       (mcons (mcar list) acc)))))
  (dedup-iter list '()))


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
(#%provide dedup)
