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

(define (mul-interval x y)
  (let ((p1 (percent x))
        (p2 (percent y))
        (factor (/ (* (center x) (center y))
                   (* 100 100))))
    (make-interval (* (- 100 p1)
                      (- 100 p2)
                      factor)
                   (* (+ 100 p1)
                      (+ 100 p2)
                      factor))))


(define (mul-interval-orig x y)
  (let ((p1 (* (lower-bound x)
               (lower-bound y)))
        (p2 (* (lower-bound x)
               (upper-bound y)))
        (p3 (* (upper-bound x)
               (lower-bound y)))
        (p4 (* (upper-bound x)
               (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (interval= a b)
  (and (= (upper-bound a) (upper-bound b))
       (= (lower-bound a) (lower-bound b))))


(define (test)
  (let ((a (make-center-percent 5 10))
        (b (make-center-percent 20 5)))
    (if (interval= (mul-interval a b)
                   (mul-interval-orig a b))
        (display "Test passed")
        (display "Test failed")))
  (newline))
