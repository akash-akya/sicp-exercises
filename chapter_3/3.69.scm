#lang sicp
(#%require "utils.scm")

;; Exercise 3.69: Write a procedure triples that takes three infinite
;; streams, S , T , and U , and produces the stream of triples ( S i ,
;; T j , U k ) such that i ≤ j ≤ k . Use triples to generate the
;; stream of all Pythagorean triples of positive integers, i.e., the
;; triples ( i , j , k ) such that i ≤ j and i 2 + j 2 = k 2 .

(#%require "stream-utils.scm")

(define ones (cons-stream 1 ones))

(define integers
  (cons-stream 1 (add-streams ones integers)))

(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream
       (stream-car s1)
       (interleave s2 (stream-cdr s1)))))

(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x)
                  (list (stream-car s) x))
                (stream-cdr t))
    (pairs (stream-cdr s) (stream-cdr t)))))

(define (triples s t u)
  (cons-stream
   (list (stream-car s) (stream-car t) (stream-car u))
   (interleave
    (stream-map (lambda (l)
                  (cons (stream-car s) l))
                (stream-cdr (pairs t u)))
    (triples (stream-cdr s)
             (stream-cdr t)
             (stream-cdr u)))))

(define pythagorean-triples
  (stream-filter
   (lambda (triple)
     (let ((a (car triple))
           (b (cadr triple))
           (c (caddr triple)))
       (= (+ (square a) (square b))
          (square c))))
   (triples integers integers integers)))

;; test
(define (test)
  (test-equal (stream-ref pythagorean-triples 0) '(3 4 5))
  (test-equal (stream-ref pythagorean-triples 1) '(6 8 10)))
