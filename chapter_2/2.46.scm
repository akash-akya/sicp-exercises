#lang sicp

;; Exercise 2.46: A two-dimensional vector v running from the origin
;; to a point can be represented as a pair consisting of an x
;; -coordinate and a y -coordinate. Implement a data abstraction for
;; vectors by giving a constructor make-vect and corresponding
;; selectors xcor-vect and ycor-vect. In terms of your selectors and
;; constructor, implement procedures add-vect, sub-vect, and
;; scale-vect that perform the operations vector addition, vector
;; subtraction, and multiplying a vector by a scalar:

;; (x1,y1) + (x2,y2) = (x1+x2, y1+y2),
;; (x1,y1) − (x2,y2) = (x1−x2, y1−y2),
;; s⋅(x,y) = (sx,sy)

(define (make-vect x y)
  (cons x y))

(define (xcor-vect v)
  (car v))

(define (ycor-vect v)
  (cdr v))


(define (add-vect a b)
  (cons (+ (xcor-vect a)
           (xcor-vect b))
        (+ (ycor-vect a)
           (ycor-vect b))))

(define (sub-vect a b)
  (cons (- (xcor-vect a)
           (xcor-vect b))
        (- (ycor-vect a)
           (ycor-vect b))))

(define (scale-vect s a)
  (cons (* (xcor-vect a) s)
        (+ (ycor-vect a) s)))
