#lang sicp
(#%require "utils.scm")

;; Exercise 5.21: Implement register machines for the following
;; procedures. Assume that the list-structure memory operations are
;; available as machine primitives.

    ;; Recursive count-leaves:

    ;; (define (count-leaves tree)
    ;;   (cond ((null? tree) 0)
    ;;         ((not (pair? tree)) 1)
    ;;         (else
    ;;          (+ (count-leaves (car tree))
    ;;             (count-leaves (cdr tree))))))


(define count-leaves
  (make-machine
   '(tree t result continue)
   '(list (list '+ +) (list 'not not))
   '((assign continue (label done))
     ;; (assign result (const 0))

    test-null
     (test (op null?) (reg tree))
     (branch (label add-zero))

     (assign t (op pair?) (reg tree))
     (test (op not) (reg t))
     (branch (label add-one))

     (save continue)
     (save result)
     (save tree)
     (assign tree (op car) (reg tree))
     (assign continue (label count-cdr))
     (goto (label test-null))

    count-cdr
     (assign t (reg result))
     (restore result)
     (assign result (op +) (reg result) (reg t))
     (save result)

     (restore tree)
     (assign tree (op cdr) (reg tree))
     (assign continue (label add-cdr))
     (goto (label test-null))

    add-cdr
     (assign t (reg result))
     (restore result)
     (assign result (op +) (reg result) (reg t))

     (restore continue)
     (goto (reg continue))

    add-zero
     (assign (reg result) (const 0))
     (goto (reg continue))

    add-one
     (assign (reg result) (const 1))
     (goto (reg continue))

    done)))
