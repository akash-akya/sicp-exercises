#lang sicp

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
