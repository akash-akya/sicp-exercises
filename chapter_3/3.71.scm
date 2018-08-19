#lang sicp
(#%require "utils.scm")
(#%require "stream-utils.scm")
;; Exercise 3.71: Numbers that can be expressed as the sum of two
;; cubes in more than one way are sometimes called Ramanujan numbers,
;; in honor of the mathematician Srinivasa Ramanujan.198 Ordered
;; streams of pairs provide an elegant solution to the problem of
;; computing these numbers. To find a number that can be written as
;; the sum of two cubes in two different ways, we need only generate
;; the stream of pairs of integers ( i , j ) weighted according to the
;; sum i 3 + j 3 (see Exercise 3.70), then search the stream for two
;; consecutive pairs with the same weight. Write a procedure to
;; generate the Ramanujan numbers. The first such number is
;; 1,729. What are the next five?

(define (merge-weighted s1 s2 weight)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (cond ((<= (weight s1car) (weight s2car))
                  (cons-stream
                   s1car
                   (merge-weighted (stream-cdr s1) s2 weight)))
                 (else
                  (cons-stream
                   s2car
                   (merge-weighted s1 (stream-cdr s2) weight))))))))

(define (weighted-pairs s t weight)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (merge-weighted
    (stream-map (lambda (x)
                  (list (stream-car s) x))
                (stream-cdr t))
    (weighted-pairs (stream-cdr s) (stream-cdr t) weight)
    weight)))


(define (weight-func p) (+ (cube (car p)) (cube (cadr p))))

(define cube-sum-pairs
  (weighted-pairs integers integers weight-func))

(define (ramanujan-pairs s)
  (if (= (weight-func (stream-car s))
         (weight-func (stream-car (stream-cdr s))))
      (cons-stream (stream-car s)
                   (ramanujan-pairs (stream-cdr s)))
      (ramanujan-pairs (stream-cdr s))))

(define ramanujan-pairs-stream
  (ramanujan-pairs cube-sum-pairs))

;; test
(define (test)
  (test-equal (take-n ramanujan-pairs-stream 5)
    '((1 12) (2 16) (2 24) (10 27) (4 32))))
