#lang sicp
(#%require "utils.scm")

;;  Exercise 5.22: Exercise 3.12 of 3.3.1 presented an append
;;  procedure that appends two lists to form a new list and an append!
;;  procedure that splices two lists together. Design a register
;;  machine to implement each of these procedures. Assume that the
;;  list-structure memory operations are available as primitive
;;  operations.


(define append
  (make-machine
   '(x y t result continue)
   '(list (list '+ +) (list 'not not))
   '((assign continue (label done))
     ;; (assign result (const 0))

    append-test
     (test (op null?) (reg x))
     (branch (label return-y))

     (save x)
     (assign x (op cdr) (reg x))
     (save continue)
     (assign continue (label append-cons))
     (goto (label append-test))

    append-cons
     (restore x)
     (assign t (op car) (reg x))
     (assign result (op cons) (reg t) (reg result))

     (restore continue)
     (goto (reg continue))

    return-y
     (assign result (reg y))
     (goto (reg continue))

     done)))

(define append!
  (make-machine
   '(x y t result continue)
   '(list (list '+ +) (list 'not not))
   '((assign continue (label done))
     (save x)

    test-null
     (assign t (op cdr) (reg x))
     (test (op null?) (reg t))
     (branch (label return-x))

     (assign x (reg t))
     (goto (label test-null))

    return-x
     (assign result x)
     (goto (reg continue))

    done
     (perform (op set-cdr!) (reg result) (reg y))
     (restore x))))



;; test
