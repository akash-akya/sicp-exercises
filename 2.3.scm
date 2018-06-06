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

(define (diff p1 p2)
  (abs (- p1 p2)))

(define (square a)
  (* a a))

(define (segment-length segment)
  (let ((a (start-segment segment))
        (b (end-segment segment)))
    (sqrt (+ (square
              (* (diff (x-point a) (x-point b))))
             (square
              (* (diff (y-point a) (y-point b))))))))

(define (midpoint-segment segment)
  (mid-point (start-segment segment)
             (end-segment segment)))

;; rectangle operations
(define (area rectangle)
  (* (rectangle-width rectangle)
     (rectangle-length rectangle)))

(define (perimeter rectangle)
  (* 2
     (+ (rectangle-width rectangle)
        (rectangle-length rectangle))))


;; rectangle version 1
(define (make-rectangle seg1 seg2 seg3 seg4)
  (cons seg1
        (cons seg2
              (cons seg3
                    seg4))))

(define (rectangle-width rectangle)
  (segment-length (car rectangle)))

(define (rectangle-length rectangle)
  (segment-length (car (cdr rectangle))))

(define (test)
  (let ((seg1 (make-segment (make-point -5  5) (make-point  5  5)))
        (seg2 (make-segment (make-point  5  5) (make-point  5 -5)))
        (seg3 (make-segment (make-point  5 -5) (make-point -5 -5)))
        (seg4 (make-segment (make-point -5 -5) (make-point -5  5))))
    (if (and
         (= (area (make-rectangle seg1 seg2 seg3 seg4))
            100)
         (= (perimeter (make-rectangle seg1 seg2 seg3 seg4))
            40))
        (display "Tests passed")
        (display "Tests failed!")))
  (newline))


;; Rectangle version 2
(define (make-rectangle topleft bottom-right)
  (cons topleft bottom-right))

(define (rectangle-width rectangle)
  (diff (x-point (car rectangle))
        (x-point (cdr rectangle))))

(define (rectangle-length rectangle)
  (diff (y-point (car rectangle))
        (y-point (cdr rectangle))))

(define (test)
  (let ((topleft (make-point -5 5))
        (bottom-right (make-point 5 -5)))
    (if (and
         (= (area (make-rectangle topleft bottom-right))
            100)
         (= (perimeter (make-rectangle topleft bottom-right))
            40))
        (display "Tests passed")
        (display "Tests failed!")))
  (newline))
