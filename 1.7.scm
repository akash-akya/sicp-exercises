#lang sicp

(define (average a b)
  (/ (+ a b) 2))

(define (square-root x)
  (define good-enough?
    (let ((prev 0))
      (lambda (guess)
        (define prev-guess prev)
        (set! prev guess)
        (< (abs (- guess prev-guess))
           0.0000001))))

  (define (nw-square-root x guess)
    (if (good-enough? guess)
        guess
        (nw-square-root x (average guess (/ x guess)))))

  (nw-square-root x 1.0))

