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



;; different methods to find the sqrt
(define (sqrt1 x)
  (newton-method
   (lambda (y) (- (square y) x))
   1.0))

(define (sqrt2 x)
  (newton-method
   (lambda (y) (- (/ x (square y)) 1))
   1.0))

(define (sqrt3 x)
  (newton-method
   (lambda (y) (- (/ x y) y))
   1.0))
