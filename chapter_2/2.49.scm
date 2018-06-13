#lang sicp
(#%require sicp-pict)

;; Exercise 2.49: Use segments->painter to define the following primitive painters:

;; 1. The painter that draws the outline of the designated frame.
;; 2. The painter that draws an “X” by connecting opposite corners of the frame.
;; 3. The painter that draws a diamond shape by connecting the midpoints of the sides of the frame.
;; 4. The wave painter.

;; 1
(define draw-outline
  (let ((tl (make-vect 0    0.99))
        (tr (make-vect 0.99 0.99))
        (bl (make-vect 0    0))
        (br (make-vect 0.99 0)))
    (let ((left   (make-segment bl tl))
          (top    (make-segment tl tr))
          (right  (make-segment tr br))
          (bottom (make-segment br bl)))
      (segments->painter (list left right top bottom)))))


;; 2
(define draw-x
  (let ((tl (make-vect 0 1))
        (tr (make-vect 1 1))
        (bl (make-vect 0 0))
        (br (make-vect 1 0)))
    (let ((line1 (make-segment bl tr))
          (line2 (make-segment tl br)))
      (segments->painter (list line1 line2)))))


;; 3
(define diamond
  (let ((tl (make-vect 0    0.49))
        (tr (make-vect 0.49 0.99))
        (bl (make-vect 0.49 0   ))
        (br (make-vect 0.99 0.49)))
    (let ((left   (make-segment bl tl))
          (top    (make-segment tl tr))
          (right  (make-segment tr br))
          (bottom (make-segment br bl)))
      (segments->painter (list left right top bottom)))))


(define (test-outline)
  (paint draw-outline))

(define (test-x)
  (paint draw-x))


(define (test-diamon)
  (paint diamond))
