#lang sicp


;; Exercise 1.29: Simpson’s Rule is a more accurate method of numerical integration than the method illustrated above. Using Simpson’s Rule, the integral of a function f between a and b is approximated as
;; h 3 ( y 0 + 4 y 1 + 2 y 2 + 4 y 3 + 2 y 4 + ⋯ + 2 y n − 2 + 4 y n − 1 + y n ) ,
;; where h = ( b − a ) / n , for some even integer n , and y k = f ( a + k h ) . (Increasing n increases the accuracy of the approximation.) Define a procedure that takes as arguments f , a , b , and n and returns the value of the integral, computed using Simpson’s Rule. Use your procedure to integrate cube between 0 and 1 (with n = 100 and n = 1000 ), and compare the results to those of the integral procedure shown above.


(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))


(define (simpson f a b n)
  (define h (/ (- b a) n))

  (define (get-coeff k)
    (cond ((= k 0) 1)
          ((= k n) 1)
          ((odd? k) 4)
          (else 2)))

  (define (yk k) (f (+ a (* h k))))

  (define (term i)
    (* (get-coeff i)
       (yk i)))

  (/ (* h (sum term 0 inc n)) 3.0))


(define (test-simpson)
  (define (cube x) (* x x x))
  (simpson cube 0 1 100))


;;; Integral by normal method
(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))
