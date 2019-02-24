#lang sicp
(#%require "utils.scm")
(#%require rackunit)
(#%require rackunit/text-ui)

;; Exercise 3.16: Ben Bitdiddle decides to write a procedure to count
;; the number of pairs in any list structure. “It’s easy,” he
;; reasons. “The number of pairs in any structure is the number in the
;; car plus the number in the cdr plus one more to count the current
;; pair.” So Ben writes the following procedure:

;; (define (count-pairs x)
;;   (if (not (pair? x))
;;       0
;;       (+ (count-pairs (car x))
;;          (count-pairs (cdr x))
;;          1)))

;; Show that this procedure is not correct. In particular, draw
;; box-and-pointer diagrams representing list structures made up of
;; exactly three pairs for which Ben’s procedure would return 3;
;; return 4; return 7; never return at all.

(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))


;; test

(test-equal (count-pairs '(1 2 3)) 3)


(define a '(1))

(define b '(2))

(define c '(3))

(define test-list (append a (append b c)))
(test-equal (count-pairs test-list) 3)

(display (set-car! test-list b))
(newline)

;; (set! test-list (set-car! test-list b))

(define tests
  (test-suite
   "Dummy tests"

   (check-equal? 1 1 "Something true")
   (check-equal? 2 2 "Something false")))

(run-tests tests)
