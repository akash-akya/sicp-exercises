#lang sicp

(define (compute-rec n)
  (cond ((< n 3) n)
        (else (+ (compute-rec (- n 1))
                 (* 2 (compute-rec (- n 2)))
                 (* 3 (compute-rec (- n 3)))))))


(define (iter-helper p1 p2 p3 n)
  (cond ((= n 0) p3)
        (else (iter-helper p2
                           p3
                           (+ p3 (* 2 p2) (* 3 p1))
                           (- n 1)))))
(define (compute-iter n)
  (if (< n 3)
      n
      (iter-helper 0 1 2 n)))
