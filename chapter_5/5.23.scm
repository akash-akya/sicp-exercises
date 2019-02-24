#lang sicp
(#%require "utils.scm")

;;  Exercise 5.23: Extend the evaluator to handle derived expressions
;;  such as cond, let, and so on (4.1.2). You may “cheat” and assume
;;  that the syntax transformers such as cond->if are available as
;;  machine operations.313


'(ev-cond
  (assign exp (op cond->if) (reg exp))
  (goto (label eval-dispatch)))



;; test
