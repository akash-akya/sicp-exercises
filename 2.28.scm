#lang sicp

;; Exercise 2.28: Write a procedure fringe that takes as argument a
;; tree (represented as a list) and returns a list whose elements are all
;; the leaves of the tree arranged in left-to-right order. For example,

;;     (define x (list (list 1 2) (list 3 4)))

;;     (fringe x) (1 2 3 4)

;;     (fringe (list x x)) (1 2 3 4 1 2 3 4)


(define (fringe tree)
  (cond ((null? tree) nil)
        ((not (pair? tree))
         (list tree))
        (else
         (append (fringe (car tree))
                 (fringe (cdr tree))))))

(define (test)
  (define x (list (list 1 2) (list 3 4)))
  (if (equal? (fringe (list x x))
              (list 1 2 3 4 1 2 3 4))
      (display "Test passed")
      (display "Test failed"))
  (newline))
