#lang sicp

(define (cont-frac n d k)
  (define (frac i)
    (if (= i k)
        (/ (n i) (d i))
        (/ (n i) (- (d i)
                    (frac (+ i 1))))))
  (frac 1))


;; tan
(define (tan-cf x k)
  (define (nextd i) (- (* 2 i) 1))
  (define (nextn i) (if (= i 1) x (* x x)))
  (cont-frac nextn nextd k))
