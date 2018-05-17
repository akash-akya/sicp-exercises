#lang sicp

;; Coin change
(define (value type)
  (cond ((= type 1) 1)
        ((= type 2) 5)
        ((= type 3) 10)
        ((= type 4) 25)
        ((= type 5) 50)))

(define (count-change amt types)
  (cond ((= amt 0) 1)
        ((or (= types 0)
             (< amt 0))
         0)
        (else (+ (count-change amt (- types 1))
                 (count-change (- amt (value types))
                               types)))))
