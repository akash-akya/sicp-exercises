#lang sicp
(#%require "utils.scm")
(#%require "stream-utils.scm")

;;  Exercise 3.76: Eva Lu Ator has a criticism of Louis’s approach in
;;  Exercise 3.75. The program he wrote is not modular, because it
;;  intermixes the operation of smoothing with the zero-crossing
;;  extraction. For example, the extractor should not have to be
;;  changed if Alyssa finds a better way to condition her input
;;  signal. Help Louis by writing a procedure smooth that takes a
;;  stream as input and produces a stream in which each element is the
;;  average of two successive input stream elements. Then use smooth
;;  as a component to implement the zero-crossing detector in a more
;;  modular style.

(define (dummy-stream list)
  (cons-stream
   (car list)
   (dummy-stream (cdr list))))

(define sense-data (dummy-stream '(1 2 1.5 1 0.5 -0.1 -2 -3 -2 -0.5 0.2 3 4)))

(define (sign-change-detector current last)
  (cond ((and (< last 0) (> current 0)) 1)
        ((and (> last 0) (< current 0)) -1)
        (else 0)))

(define (smooth stream)
  (cons-stream
   (/ (+ (stream-car stream) (stream-car (stream-cdr stream)))
      2)
   (smooth (stream-cdr stream))))

(define zero-crossings
  (stream-map sign-change-detector
              (smooth sense-data)
              (cons-stream 0 sense-data)))


;; test
(define (test)
  (test-equal (take-n zero-crossings 10) '(0 0 0 0 0 -1 0 0 0 0)))
