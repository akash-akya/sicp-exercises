#lang sicp
(#%require "utils.scm")

;; Exercise 4.1: Notice that we cannot tell whether the metacircular
;; evaluator evaluates operands from left to right or from right to
;; left. Its evaluation order is inherited from the underlying Lisp:
;; If the arguments to cons in list-of-values are evaluated from left
;; to right, then list-of-values will evaluate operands from left to
;; right; and if the arguments to cons are evaluated from right to
;; left, then list-of-values will evaluate operands from right to
;; left.

;; Write a version of list-of-values that evaluates operands from left
;; to right regardless of the order of evaluation in the underlying
;; Lisp. Also write a version of list-of-values that evaluates
;; operands from right to left.


(define (my-eval exp env)
  (cond ((self-evaluating? exp)
         exp)
        ((variable? exp)
         (lookup-variable-value exp env))
        ((quoted? exp)
         (text-of-quotation exp))
        ((assignment? exp)
         (eval-assignment exp env))
        ((definition? exp)
         (eval-definition exp env))
        ((if? exp)
         (eval-if exp env))
        ((lambda? exp)
         (make-procedure
          (lambda-parameters exp)
          (lambda-body exp)
          env))
        ((begin? exp)
         (eval-sequence
          (begin-actions exp)
          env))
        ((cond? exp)
         (my-eval (cond->if exp) env))
        ((application? exp)
         (my-apply (my-eval (operator exp) env)
                (list-of-values
                 (operands exp)
                 env)))
        (else
         (error "Unknown expression
                 type: EVAL" exp))))



(define (my-apply procedure arguments)
  ;; (display procedure) (newline)
  ;; (display arguments) (newline)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure
          procedure
          arguments))
        ((compound-procedure? procedure)
         (eval-sequence
           (procedure-body procedure)
           (extend-environment
             (procedure-parameters
              procedure)
             arguments
             (procedure-environment
              procedure))))
        (else
         (error "Unknown procedure
                 type: APPLY"
                procedure))))


(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (my-eval (first-operand exps) env)
            (list-of-values
             (rest-operands exps)
             env))))

(define (eval-if exp env)
  (if (true? (my-eval (if-predicate exp) env))
      (my-eval (if-consequent exp) env)
      (my-eval (if-alternative exp) env)))

(define (eval-sequence exps env)
  (cond ((last-exp? exps)
         (my-eval (first-exp exps) env))
        (else
         (my-eval (first-exp exps) env)
         (eval-sequence (rest-exps exps)
                        env))))

(define (eval-assignment exp env)
  (set-variable-value!
   (assignment-variable exp)
   (my-eval (assignment-value exp) env)
   env)
  'ok)

(define (eval-definition exp env)
  (define-variable!
    (definition-variable exp)
    (my-eval (definition-value exp) env)
    env)
  'ok)

;; Added

(define (nth list n)
  (if (= n 0)
      (car list)
      (nth (cdr list) (- n 1))))

(define primitives '(+ - * > < =))

(define (find-primitive-procedures symbol)
  (define (iter prims procedures)
    (if (eq? (car prims) symbol)
        (car procedures)
        (iter (cdr prims) (cdr procedures))))
  (iter primitives (list + - * > < =)))

(define (self-evaluating? exp)
  (or (number? exp)
      (string? exp)
      (member exp primitives)))

(define special-forms '(if cond define set! lambda begin quote))
(define (variable? exp)
  (and (symbol? exp)
       (not (member exp special-forms))))



;; Frame is a cons of variable(params) and associated values (args): (variables . values)
(define (make-frame params args)
  (cons params args))

(define (params-from-frame frame)
  (car frame))

(define (args-from-frame frame)
  (cdr frame))

(define (get-frame env)
  (car env))



(define (lookup-in-frame frame variable)
  (define (lookup params args)
    (cond ((null? params) 'not-found)
          ((eq? (car params) variable)
           (car args))
          (else
           (lookup (cdr params) (cdr args) variable))))
  (lookup-in-frame (params-from-frame frame) (args-from-frame frame)))

(define (lookup-variable-value variable env)
  (cond ((null? env)
         (error "Variable not found"))
        ((lookup-in-frame (get-frame env) variable)
         (lookup-in-frame (get-frame env) variable))
        (else
         (lookup-variable-value variable (cdr env)))))

(define (function exp) (car exp))

(define (arguments exp) (cdr exp))

(define (quoted? exp)
  (eq? (function exp) 'quote))

(define (text-of-quotation exp)
  (nth (arguments exp) 0))

(define (assignment? exp)
  (eq? (function exp) 'set!))

(define (definition? exp)
  (eq? (function exp) 'define))

(define (if? exp)
  (eq? (function exp) 'if))

(define (lambda? exp)
  (eq? (function exp) 'lambda))



;; Procedure is just a ordered list
(define (make-procedure params body env)
  (list env params body))

(define (procedure-parameters procedure)
  (nth procedure 1))

(define (procedure-body procedure)
  (list (nth procedure 2)))

(define (procedure-environment procedure)
  (nth procedure 0))



(define (lambda-parameters exp)
  (nth (arguments exp) 0))

(define (lambda-body exp)
  (cadr (arguments exp)))

(define (begin? exp)
  (eq? (function exp) 'begin))

(define (begin-actions exp)
  (arguments exp))

(define (cond? exp)
  (eq? (function exp) 'cond))

;; if structure
(define (make-if predicate consequence alterntive)
  (list 'if predicate
        consequence
        alterntive))

(define (if-predicate exp)
  (nth exp 1))

(define (if-consequent exp)
  (nth exp 2))

(define (if-alternative exp)
  (nth exp 3))


(define (cond->if exp)
  (define (cond-helper exp)
    (if (null? exp)
        '()
        (let ((first-cond (car exp))
              (rest-cond (cdr exp)))
          (make-if
           (nth first-cond 0)
           (nth first-cond 1)
           (cond-helper rest-cond)))))

  ;; ignore `cond' in (cond <body>)
  (cond-helper (cdr exp)))

(define (cons? exp)
  (list? exp))

(define (application? exp)
  (cons? exp))

(define (operator exp)
  (car exp))

(define (operands exp)
  (cdr exp))

(define (primitive-procedure? procedure)
  (member procedure primitives))

; Cheating!
(define (apply-primitive-procedure procedure arguments)
  (apply (find-primitive-procedures procedure) arguments)
  ;; (define (add arguments)
  ;;   (apply + arguments))

  ;; (define (subtract arguments)
  ;;   (apply - arguments))

  ;; (cond ((eq? procedure '+) (add arguments))
  ;;       ((eq? procedure '-) (subtract arguments)))
  )

(define (compound-procedure? procedure)
  (cons? (function procedure)))

(define (extend-environment params args env)
  (cons (make-frame params args) env))


(define (no-operands? exp)
  (eq? exp '()))

(define (first-operand exps)
  (car exps))

(define (rest-operands exps)
  (cdr exps))

(define (true? exp)
  (eq? exp #t))


(define (last-exp? exps)
  (null? (cdr exps)))

(define (first-exp exps)
  (car exps))

(define (rest-exps exps)
  (cdr exps))

(define (set-variable-value! variable value env)
  (if (update-frame (car env) variable value)
      'ok
      (set-variable-value! variable value (cdr env))))

(define (update-frame frame variable value)
  (define (update-frame-helper params args)
    (cond ((null? params) #f)
          ((eq? (car params) variable)
           (set-car! args value)
           #t)
          (else
           (update-frame-helper (cdr params) (cdr args)))))

  (update-frame-helper (car frame) (cdr frame)))


(define (assignment-variable exp)
  (nth (arguments exp) 0))

(define (assignment-value exp)
  (nth (arguments exp) 1))


(define (define-variable! variable value env)
  (set-car! env
            (make-frame
             (cons variable (params-from-frame (get-frame env)))
             (cons value (args-from-frame (get-frame env))))))

(define (definition-variable exp)
  (nth (arguments exp) 0))

(define (definition-value exp)
  (nth (arguments exp) 1))

(define empty-env '((() ()) ()))

;; test
