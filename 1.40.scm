#lang sicp

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (closs-enough? x y)
    (< (abs (- x y)) tolerance))

  (define (try guess)
    (let ((new-guess (f guess)))
      (if (closs-enough? guess new-guess)
          new-guess
          (try (f new-guess)))))

  (try first-guess))



(define dx 0.00001)

(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))


(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x)
            ((deriv g) x)))))

(define (newton-method g guess)
  (fixed-point (newton-transform g)
               guess))

(define (square x) (* x x))
(define (cube x) (* (square x) x))


;; Soution
(define (cubic a b c)
  "Returns the cubic eqation of the form f(x) = x³+ax²+bx+c = 0"
  (lambda (x)
    (+ (cube x)
       (* a (square x))
       (* b x)
       c)))

(define (cubic-solver a b c)
  (newton-method
   (cubic a b c)
   -1.0))

(cubic-solver -3 3 -1)
