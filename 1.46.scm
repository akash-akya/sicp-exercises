#lang sicp
;; Exercise 1.46: Several of the numerical methods described in this
;; chapter are instances of an extremely general computational strategy
;; known as iterative improvement. Iterative improvement says that, to
;; compute something, we start with an initial guess for the answer, test
;; if the guess is good enough, and otherwise improve the guess and
;; continue the process using the improved guess as the new guess. Write
;; a procedure iterative-improve that takes two procedures as arguments:
;; a method for telling whether a guess is good enough and a method for
;; improving a guess. Iterative-improve should return as its value a
;; procedure that takes a guess as argument and keeps improving the guess
;; until it is good enough. Rewrite the sqrt procedure of 1.1.7 and the
;; fixed-point procedure of 1.3.3 in terms of iterative-improve.

(define (iterative-improvement good-enough? improve)
  (define (try guess)
    (if (good-enough? guess)
        guess
        (try (improve guess))))
  try)


(define (sqrt-good-enough? x)
  (lambda (guess)
    (< (abs (- (* guess guess) x))
       0.00001)))

(define (sqrt x)
  ((iterative-improvement
    (sqrt-good-enough? x)
    (lambda (guess)
      (/ (+ guess (/ x guess))
         2)))
   1.0))



(define (fixed-point-enough? f)
  (lambda (guess)
    (< (abs (- guess (f guess)))
       0.00001)))


(define (fixed-point f guess)
  ((iterative-improvement
    (fixed-point-enough? f)
    (lambda (g)
      (f g)))
   guess))


(define (new-sqrt n)
  (fixed-point
   (lambda (y)
     (/ (+ y (/ n y))
        2.0))
   1.0))
