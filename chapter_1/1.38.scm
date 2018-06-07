#lang sicp

(define (cont-frac n d k)
  (define (frac i)
    (if (= i k)
        (/ (n i) (d i))
        (/ (n i) (+ (d i)
                    (frac (+ i 1))))))
  (frac 1))


;; e-2
(+ (cont-frac (lambda (x) 1.0)
              (lambda (x)
                (let ((div (quotient (+ 1 x) 3))
                      (rem (remainder (+ 1 x) 3)))
                  (if (= rem 0)
                      (* 2 div)
                      1)))
              10)
   2)
