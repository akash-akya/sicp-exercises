#lang sicp

(define (make-interval a b) (cons a b))

(define (upper-bound a) (car a))

(define (lower-bound a) (cdr a))

(define (center i)
  (/ (+ (lower-bound i)
        (upper-bound i))
     2))

(define (make-center-percent center percent)
  (let ((width (* (/ percent 100) center)))
    (make-interval (+ center width) (- center width))))

(define (percent interval)
  (* 100
     (/ (/ (- (upper-bound interval) (lower-bound interval))
           2)
        (center interval))))
