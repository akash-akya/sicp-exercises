#lang sicp
(#%require "utils.scm")

;; Exercise 3.17: Devise a correct version of the count-pairs
;; procedure of Exercise 3.16 that returns the number of distinct
;; pairs in any structure. (Hint: Traverse the structure, maintaining
;; an auxiliary data structure that is used to keep track of which
;; pairs have already been counted.)

(define (count-pairs x)
  (define visited '())
  (define (iter a)
    (cond ((not (pair? x)) 0)
          ((not (member x visited))
           (set! visited (cons x visited))
           (+ (count-pairs (car x))
              (count-pairs (cdr x))
              1))
          (else 0)))
  (iter x))


;; test
