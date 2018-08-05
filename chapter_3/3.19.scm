#lang sicp
(#%require "utils.scm")

;; Exercise 3.19: Redo Exercise 3.18 using an algorithm that takes
;; only a constant amount of space. (This requires a very clever
;; idea.)

(define (check-cycle list)
  (if (null? list)
      #f
      (let
          ((a list)
           (b (cdr list)))
          (define (iter)
            (cond ((null? b) #f)
                  ((eq? a b) #t)
                  ((null? (cdr b)) #f)
                  ((eq? a (cdr b)) #t)
                  (else (set! a (cdr a))
                        (set! b (cdr (cdr b)))
                        (iter))))
          (iter))))

;; test

(define a '())
(test-equal (check-cycle a) #f)

(set! a '(1))
(test-equal (check-cycle a) #f)

(set-cdr! a a)
(test-equal (check-cycle a) #t)
