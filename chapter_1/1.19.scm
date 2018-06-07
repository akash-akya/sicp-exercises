#lang sicp

(define (fib n)
  (fib-iter 1 0 0 1 n))

(define (sq a) (* a a))

(define (cb a) (* a a a))

(define (comp-p p q)
  (+ (sq p)
     (* 2 p (sq q))
     (* p (cb q))))

(define (comp-q p q)
  (* q
     (+ (* (sq q)
           (+ 1 p (sq p)))
        (* p
           (+ (* 2 p) q)))))

(define (fib-iter a b p q count)
  (cond ((= count 0) 
         b)
        ((even? count)
         (fib-iter a
                   b
                   (comp-p p q)
                   (comp-q p q)
                   (/ count 2)))
        (else 
         (fib-iter (+ (* b q) 
                      (* a q) 
                      (* a p))
                   (+ (* b p) 
                      (* a q))
                   p
                   q
                   (- count 1)))))
