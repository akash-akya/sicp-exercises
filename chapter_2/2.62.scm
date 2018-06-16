#lang sicp
(#%require "utils.scm")

;; Exercise 2.62: Give a Θ ( n ) implementation of union-set for sets
;; represented as ordered lists.

(define (union-set set1 set2)
  (if (null? set1)
      set2
      (let ((x (car set1))
            (y (car set2)))
        (cond ((= x y)
               (cons x (union-set (cdr set1) (cdr set2))))
              ((< x y)
               (cons x (union-set (cdr set1) set2)))
              (else
               (cons y (union-set set1 (cdr set2))))))))

;; test
(test-equal (union-set '(1 3 4 5 6) '(0 1 2 3 6))
  '(0 1 2 3 4 5 6))
