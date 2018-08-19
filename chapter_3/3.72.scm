#lang sicp
(#%require "utils.scm")
(#%require "stream-utils.scm")

;; Exercise 3.72: In a similar way to Exercise 3.71 generate a stream
;; of all numbers that can be written as the sum of two squares in
;; three different ways (showing how they can be so written).

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


(define (weight-func p) (+ (square (car p)) (square (cadr p))))

(define square-sum-pairs
  (weighted-pairs integers integers weight-func))

(define (sq-pairs s)
  (let ((a (stream-car s))
        (b (stream-car (stream-cdr s)))
        (c (stream-car (stream-cdr (stream-cdr s)))))

    (if (= (weight-func a)
           (weight-func b)
           (weight-func c))
        (cons-stream (list a b c)
                     (sq-pairs (stream-cdr s)))
        (sq-pairs (stream-cdr s)))))

(define sq-pairs-stream
  (sq-pairs square-sum-pairs))


;; test
(define (test)
  (define pairs (take-n sq-pairs-stream 5))
  (define (show p)
    (if (null? p)
        (newline)
        (begin
          (let ((numbers (car p)))
            (display (car numbers)) (display "\t")
            (display (cadr numbers)) (display "\t")
            (display (caddr numbers))
            (newline))
          (show (cdr p)))))

  (show pairs))
