#lang sicp
(#%require "utils.scm")
(#%require "stream-utils.scm")

;; Exercise 3.77: The integral procedure used above was analogous to
;; the “implicit” definition of the infinite stream of integers in
;; 3.5.2. Alternatively, we can give a definition of integral that is
;; more like integers-starting-from (also in 3.5.2):

;; (define (integral
;;          integrand initial-value dt)
;;   (cons-stream
;;    initial-value
;;    (if (stream-null? integrand)
;;        the-empty-stream
;;        (integral
;;         (stream-cdr integrand)
;;         (+ (* dt (stream-car integrand))
;;            initial-value)
;;         dt))))

;; When used in systems with loops, this procedure has the same
;; problem as does our original version of integral. Modify the
;; procedure so that it expects the integrand as a delayed argument
;; and hence can be used in the solve procedure shown above.

(define (integral delayed-integrand initial-value dt)
  (cons-stream
   initial-value
   (let ((integrand (force delayed-integrand)))
     (if (stream-null? integrand)
         the-empty-stream
         (integral
          (delay (stream-cdr integrand))
          (+ (* dt (stream-car integrand))
             initial-value)
          dt)))))


(define (solve f y0 dt)
  (let ((y nil))
    (set! y (integral (delay dy) y0 dt))
    (define dy (stream-map f y))
    y))

;; test
(define (test)
  (let ((result (stream-ref (solve (lambda (y) y) 1 0.001) 1000)))
    (test-equal (< (abs (- result 2.7169239)) 0.000001) #t)))
