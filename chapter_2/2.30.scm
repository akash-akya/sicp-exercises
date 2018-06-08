#lang sicp

;; Exercise 2.30: Define a procedure square-tree analogous to the
;; square-list procedure of Exercise 2.21. That is, square-tree should
;; behave as follows:

;;     (square-tree (list 1 (list 2 (list 3 4) 5) (list 6 7))) (1 (4 (9
;; 16) 25) (36 49))

;;     Define square-tree both directly (i.e., without using any
;; higher-order procedures) and also by using map and recursion.


;; withouts using higher order function

(define (square-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (* tree tree))
        (else (cons (square-tree (car tree))
                    (square-tree (cdr tree))))))



(define (square-tree-map tree)
  (map (lambda (subtree)
         (if (not (pair? subtree))
             (* subtree subtree)
             (square-tree subtree)))
       tree))

(define (test)
  (if (equal? (square-tree
               (list 1
                     (list 2 (list 3 4) 5)
                     (list 6 7)))
              '(1 (4 (9 16) 25) (36 49)))
      (display "square-tree passed")
      (display "square-tree failed"))
  (newline)

  (if (equal? (square-tree-map
               (list 1
                     (list 2 (list 3 4) 5)
                     (list 6 7)))
              '(1 (4 (9 16) 25) (36 49)))
      (display "square-tree-map passed")
      (display "square-tree-map failed"))
  (newline))
