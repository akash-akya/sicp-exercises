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


(fixed-point cos 1)
