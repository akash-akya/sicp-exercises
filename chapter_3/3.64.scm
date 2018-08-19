#lang sicp
(#%require "utils.scm")
(#%require "stream-utils.scm")

;; Exercise 3.64: Write a procedure stream-limit that takes as
;; arguments a stream and a number (the tolerance). It should examine
;; the stream until it finds two successive elements that differ in
;; absolute value by less than the tolerance, and return the second of
;; the two elements. Using this, we could compute square roots up to a
;; given tolerance by

;; (define (sqrt x tolerance)
;;   (stream-limit (sqrt-stream x) tolerance))

(define (average a b) (/ (+ a b) 2))

(define (sqrt-improve guess x)
  (average guess (/ x guess)))

(define (sqrt-stream x)
  (define guesses
    (cons-stream
     1.0 (stream-map
          (lambda (guess)
            (sqrt-improve guess x))
          guesses)))
  guesses)

(define (stream-limit stream tolerance)
  (let ((a (stream-car stream))
        (b (stream-car (stream-cdr stream))))
    (if (< (abs (- a b))
           tolerance)
        b
        (stream-limit (stream-cdr stream)
                      tolerance))))

(define (sqrt x tolerance)
  (stream-limit (sqrt-stream x) tolerance))


;; test
(define (test)
  (let ((sqrt-of-two (sqrt 2 0.0001)))
    (define truncated (/ (truncate (* sqrt-of-two 10000)) 10000))
    (test-equal truncated 1.4142)))
