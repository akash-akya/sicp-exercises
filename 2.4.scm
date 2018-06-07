#lang sicp

(define (cons x y)
  (lambda (chooser) (chooser x y)))

(define (car z)
  (z (lambda (first second) first)))

(define (cdr z)
  (z (lambda (first second) second)))


;; Test
(define (test)
  (if (and
       (= (car (cons 3 4))
          3)
       (= (cdr (cons 3 4))
          4))
      (display "All tests are passed")
      (display "Tests failed!"))
  (newline))
