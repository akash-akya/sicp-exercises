#lang sicp
(#%require sicp-pict)

;; Exercise 2.45: Right-split and up-split can be expressed as instances of a general splitting operation. Define a procedure split with the property that evaluating

;;   (define right-split (split beside below))
;;   (define up-split (split below beside))

;; produces procedures right-split and up-split with the same behaviors as the ones already defined.


(define (split combiner action)
  (lambda (painter)
    (combiner painter (action painter painter))))

(define right-split (split beside below))

(define up-split (split below beside))

;; test
(define (test-right)
  (paint (right-split einstein)))

(define (test-up)
  (paint (up-split einstein)))
