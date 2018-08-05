#lang sicp
(#%require "utils.scm")

;; Exercise 3.18: Write a procedure that examines a list and
;; determines whether it contains a cycle, that is, whether a program
;; that tried to find the end of the list by taking successive cdrs
;; would go into an infinite loop. Exercise 3.13 constructed such
;; lists.


(define (check-cycle list)
  (define visited '())
  (define (iter list)
    (cond ((null? list) #f)
          ((member (car list) visited)
           #t)
          (else (set! visited (cons (car list)
                                    visited))
                (iter (cdr list)))))
  (iter list))

;; test

(define a '(1))
(test-equal (check-cycle a) #f)

(set-cdr! a a)
(test-equal (check-cycle a) #t)
