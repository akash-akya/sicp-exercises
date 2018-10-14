#lang sicp
(#%require sicp-pict)
(#%require "utils.scm")

;; Exercise 4.36: Exercise 3.69 discussed how to generate the stream
;; of all Pythagorean triples, with no upper bound on the size of the
;; integers to be searched. Explain why simply replacing
;; an-integer-between by an-integer-starting-from in the procedure in
;; Exercise 4.35 is not an adequate way to generate arbitrary
;; Pythagorean triples. Write a procedure that actually will
;; accomplish this. (That is, write a procedure for which repeatedly
;; typing try-again would in principle eventually generate all
;; Pythagorean triples.)

(define (require p)
  (if (not p) (amb)))

(define (an-integer-between a b)
  (require (<= a b))
  (amb a (an-integer-between (+ a 1) b)))

(define (an-integer-starting-from n)
  (amb n (an-integer-starting-from (+ n 1))))

(define (a-pythagorean-triple-between)
  (let ((low 1))
    (let ((high (an-integer-starting-from 3)))
      (let ((i (an-integer-between low high)))
        (let ((j (an-integer-between i high)))
          (let ((k (an-integer-between j high)))
            (require (= (+ (* i i) (* j j))
                        (* k k)))
            (list i j k)))))))

(define (test)
  (test-equal (a-pythagorean-triple-between) '(3 4 5)))
