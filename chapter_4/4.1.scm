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

(define (variable? exp)
  (and (symbol? exp)
       (not (member exp '(if cond define set! lambda begin quote)))))

(define (lookup-in-frame params args variable)
  (cond ((null? params) 'not-found)
        ((eq? (car params) variable)
         (car args))
        (else
         (lookup-in-frame (cdr params) (cdr args) variable))))

(define (lookup-variable-value variable env)
  (cond ((null? env) 'not-found)
        ((lookup-in-frame (car (car env)) (cdr (car env)) variable)
         (lookup-in-frame (car (car env)) (cdr (car env)) variable))
        (else
         (lookup-variable-value variable (cdr env)))))

(define (quoted? exp)
  (eq? (car exp) 'quote))

(define (text-of-quotation exp)
  (cadr exp))

(define (assignment? exp)
  (eq? (car exp) 'set!))

(define (definition? exp)
  (eq? (car exp) 'define))

(define (if? exp)
  (eq? (car exp) 'if))

(define (lambda? exp)
  (eq? (car exp) 'lambda))

(define (make-procedure params body env)
  (list env params body))

(define (lambda-parameters exp)
  (cadr exp))

(define (lambda-body exp)
  (caddr exp))

(define (begin? exp)
  (eq? (car exp) 'begin))

(define (begin-actions exp)
  (cdr exp))

(define (cond? exp)
  (eq? (car exp) 'cond))

(define (cond-helper exp)
  (if (null? exp)
      '()
      (let ((first-cond (car exp)))
        (list 'if (car first-cond)
              (cadr first-cond)
              (cond-helper (cdr exp))))))

(define (cond->if exp)
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

; cheating!
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
  (cons? (car procedure)))

(define (procedure-body procedure)
  (cddr procedure))

(define (extend-environment params args env)
  (cons (cons params args)
        env))

(define (procedure-parameters procedure)
  (cadr procedure))

(define (procedure-environment procedure)
  (car procedure))

(define (no-operands? exp)
  (eq? exp '()))

(define (first-operand exps)
  (car exps))

(define (rest-operands exps)
  (cdr exps))

(define (true? exp)
  (eq? exp #t))

(define (if-predicate exp)
  (cadr exp))

(define (if-consequent exp)
  (caddr exp))

(define (if-alternative exp)
  (caddr exp))

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
           (update-frame (cdr params)
                         (cdr args)
                         variable
                         value))))
  (update-frame-helper (car frame) (cdr frame)))


(define (assignment-variable exp)
  (cadr exp))

(define (assignment-value exp)
  (caddr exp))

(define (get-frame env)
  (car env))

(define (params-list env)
  (car env))

(define (args-list env)
  (cdr env))

(define (define-variable! variable value env)
  (set-car! env
            (cons (cons variable (params-list (get-frame env)))
                  (cons value (args-list (get-frame env))))))

(define (definition-variable exp)
  (cadr exp))

(define (definition-value exp)
  (caddr exp))

(define empty-env '((() ()) ()))

;; test
