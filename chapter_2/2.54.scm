#lang sicp

;; Exercise 2.54: Two lists are said to be equal? if they contain
;; equal elements arranged in the same order. For example,

;; (equal? '(this is a list)
;;         '(this is a list))

;; is true, but

;; (equal? '(this is a list)
;;         '(this (is a) list))

;; is false. To be more precise, we can define equal? recursively in
;; terms of the basic eq? equality of symbols by saying that a and b
;; are equal? if they are both symbols and the symbols are eq?, or if
;; they are both lists such that (car a) is equal? to (car b) and (cdr
;; a) is equal? to (cdr b). Using this idea, implement equal? as a
;; procedure.


(define (my-symbol? x)
  (not (pair? x)))

(define (my-equal? a b)
  (cond ((and (my-symbol? a)
              (my-symbol? b))
         (eq? a b))
        ((or (my-symbol? a)
             (my-symbol? b))
         #f)
        (else
         (and (my-equal? (car a) (car b))
              (my-equal? (cdr a) (cdr b))))))

;; test
(define (test)
  (cond ((not (my-equal? '(a b c d) '(a b c d)))
         (display "Test failed"))

        ((my-equal? '(a b c d) '(a b c))
         (display "Test failed"))

        ((not (my-equal? 'a 'a))
         (display "Test failed"))

        (else (display "All test passed")))
  (newline))
