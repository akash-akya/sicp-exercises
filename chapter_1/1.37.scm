#lang sicp

(define (cont-frac n d k)
  (define (frac i)
    (if (= i k)
        (/ (n i) (d i))
        (/ (n i) (+ (d i)
                    (frac (+ i 1))))))
  (frac 0))

(cont-frac (lambda (x) 1.0)
           (lambda (x) 1.0)
           10)
