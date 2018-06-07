#lang sicp

(define (double n) (+ n n))

(define (halve n) (/ n 2))

(define (mul a b acc)
  (cond ((or (= a 0) (= b 0)) 0)
        ((= b 1) acc)
        ((even? b) (mul a
                        (halve b)
                        (+ (double a) acc)))
        (else (mul a
                   (- b 1)
                   (+ a acc)))))
