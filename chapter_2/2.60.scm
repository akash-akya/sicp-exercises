#lang sicp
(#%require "utils.scm")

;; Exercise 2.60: We specified that a set would be represented as a list
;; with no duplicates. Now suppose we allow duplicates. For instance, the
;; set { 1 , 2 , 3 } could be represented as the list (2 3 2 1 3 2
;; 2). Design procedures element-of-set?, adjoin-set, union-set, and
;; intersection-set that operate on this representation. How does the
;; efficiency of each compare with the corresponding procedure for the
;; non-duplicate representation? Are there applications for which you
;; would use this representation in preference to the non-duplicate one?

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))


(define (adjoin-set x set)
  (cons x set))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2))
         '())
        ((and (not (element-of-set? (car set1) (cdr set1)))
              (element-of-set? (car set1) set2))
         (cons (car set1)
               (intersection-set (cdr set1)
                                 set2)))
        (else (intersection-set (cdr set1)
                                set2))))

(define (union set1 set2)
  (if (null? set1)
      set2
      (union (cdr set1)
             (cons (car set1) set2))))

;; test
(test-equal (element-of-set? 3 '(1 2 1 2 4 4 3 2 3 4 9))
  #t)

(test-equal (adjoin-set 1 '(1 2 1 2 4 4 3 2 3 4 9))
  '(1 1 2 1 2 4 4 3 2 3 4 9))

(test-equal (intersection-set '(5 5 1 3) '(4 5 6 3))
  '(5 3))

(test-equal (union '(5 5 4 3 3 9) '(5 5 3 0))
  '(9 3 3 4 5 5 5 5 3 0))
