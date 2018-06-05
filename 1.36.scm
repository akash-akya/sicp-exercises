#lang sicp

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (closs-enough? x y)
    (< (abs (- x y)) tolerance))

  (define (try guess)
    (display guess)
    (newline)
    (let ((new-guess (f guess)))
      (if (closs-enough? guess new-guess)
          new-guess
          (try (f new-guess)))))

  (try first-guess))


(fixed-point (lambda (x)
               (/ (log 1000)
                  (log x)))
             2)


;; with avg damping
(newline)
(display "with avg damping")
(newline)

(define (avg x y) (/ (+ x y) 2))

(fixed-point (lambda (x) (avg (/ (log 1000)
                            (log x))
                         x))
             2)
