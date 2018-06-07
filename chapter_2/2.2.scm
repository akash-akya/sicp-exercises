#lang sicp

(define (make-point x y)
  (cons x y))

(define (x-point point)
  (car point))

(define (y-point point)
  (cdr point))

(define (print-point p)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")")
  (newline))

(define (mid-point a b)
  (make-point (/ (+ (x-point a)
                    (x-point b))
                 2)
              (/ (+ (y-point a)
                    (y-point b))
                 2)))

;; segment
(define (make-segment start end)
  (cons start end))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))


(define (midpoint-segment segment)
  (mid-point (start-segment segment)
             (end-segment segment)))


;; test
(define (test)
  (let ((start (make-point -5 5))
        (end (make-point 5 -5)))
    (print-point
     (midpoint-segment
      (make-segment start end)))))
