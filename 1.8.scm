#lang sicp

(define (new-guess x guess)
  (/ (+ (* 2 guess)
        (/ x (* guess guess)))
     3))

(define (cube-root x)
  (define good-enough?
    (let ((prev 0))
      (lambda (guess)
        (define prev-guess prev)
        (set! prev guess)
        (< (abs (- guess prev-guess))
           0.0000001))))

  (define (improve x guess)
    (if (good-enough? guess)
        guess
        (improve x (new-guess x guess))))

  (improve x 1.0))

