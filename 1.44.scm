#lang sicp

;; Exercise 1.44: The idea of smoothing a function is an
;; important concept in signal processing. If f is a function and d x is
;; some small number, then the smoothed version of f is the function
;; whose value at a point x is the average of f ( x − d x ) , f ( x ) ,
;; and f ( x + d x ) . Write a procedure smooth that takes as input a
;; procedure that computes f and returns a procedure that computes the
;; smoothed f . It is sometimes valuable to repeatedly smooth a
;; function (that is, smooth the smoothed function, and so on) to obtain
;; the n-fold smoothed function. Show how to generate the n-fold smoothed
;; function of any given function using smooth and repeated from Exercise
;; 1.43.


(define (average a b c)
  (/ (+ a b c)
     3.0))

(define dx 0.00001)

(define (smooth f)
  (lambda (x)
    (average (f (- x dx))
             (f x)
             (f (+ x dx)))))

;; (define (test-smooth)
;;   (= ((smooth (lambda (x) (if (even? x) 3 0))) 2)
;;      1))

;; Second part
(define (square x) (* x x))

(define (compose f g)
  (lambda (x)
    (f (g x))))

(define (repeated f n)
  (if (= n 0)
      identity  ; apply the f 'zero' times. ie return the argument as it is
      (compose f
               (repeated f (- n 1)))))


(define (n-fold-smooth f n)
  (repeated smooth n))
