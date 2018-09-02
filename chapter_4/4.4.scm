#lang sicp
(#%require "utils.scm")

;; Exercise 4.4: Recall the definitions of the special forms and and
;; or from Chapter 1:
;; and: The expressions are evaluated from left to right. If any
;; expression evaluates to false, false is returned; any remaining
;; expressions are not evaluated. If all the expressions evaluate to
;; true values, the value of the last expression is returned. If there
;; are no expressions then true is returned.  or: The expressions are
;; evaluated from left to right. If any expression evaluates to a true
;; value, that value is returned; any remaining expressions are not
;; evaluated. If all expressions evaluate to false, or if there are no
;; expressions, then false is returned.  Install and and or as new
;; special forms for the evaluator by defining appropriate syntax
;; procedures and evaluation procedures eval-and and
;; eval-or. Alternatively, show how to implement and and or as derived
;; expressions.



(define (my-apply procedure arguments)
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

(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

(define (variable? exp) (symbol? exp))

(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (text-of-quotation exp)
  (cadr exp))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (assignment? exp)
  (tagged-list? exp 'set!))

(define (assignment-variable exp)
  (cadr exp))

(define (assignment-value exp) (caddr exp))

(define (definition? exp)
  (tagged-list? exp 'define))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda
       (cdadr exp)   ; formal parameters
       (cddr exp)))) ; body

(define (lambda? exp)
  (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

(define (make-if predicate
                 consequent
                 alternative)
  (list 'if
        predicate
        consequent
        alternative))

(define (begin? exp)
  (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))

(define (make-begin seq) (cons 'begin seq))

(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

(define (cond? exp)
  (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause)
  (car clause))
(define (cond-actions clause)
  (cdr clause))
(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))
(define (expand-clauses clauses)
  (if (null? clauses)
      'false     ; no else clause
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp
                 (cond-actions first))
                (error "ELSE clause isn't
                        last: COND->IF"
                       clauses))
            (make-if (cond-predicate first)
                     (sequence->exp
                      (cond-actions first))
                     (expand-clauses
                      rest))))))

(define (true? x)
  (not (eq? x false)))

(define (false? x)
  (eq? x false))

(define (make-procedure parameters body env)
  (list 'procedure parameters body env))
(define (compound-procedure? p)
  (tagged-list? p 'procedure))
(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))

(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())

(define (make-frame variables values)
  (cons variables values))
(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))
(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied"
                 vars
                 vals)
          (error "Too few arguments supplied"
                 vars
                 vals))))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop
              (enclosing-environment env)))
            ((eq? var (car vars))
             (car vals))
            (else (scan (cdr vars)
                        (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop
              (enclosing-environment env)))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars)
                        (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable: SET!" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (add-binding-to-frame!
              var val frame))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars)
                        (cdr vals)))))
    (scan (frame-variables frame)
          (frame-values frame))))

(define (primitive-procedure-names)
  (map car primitive-procedures))

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc)
  (cadr proc))

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list '+ +)
        (list '- -)
        (list '* *)
        (list '= =)
        (list '> >)
        (list '< <)))

(define (primitive-procedure-objects)
  (map (lambda (proc)
         (list 'primitive (cadr proc)))
       primitive-procedures))

(define (apply-primitive-procedure proc args)
  (apply (primitive-implementation proc) args))

(define (setup-environment)
  (let ((initial-env
         (extend-environment
          (primitive-procedure-names)
          (primitive-procedure-objects)
          the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))

(define the-global-environment
  (setup-environment))

(define (my-eval exp env)
  (cond ((self-evaluating? exp)
         exp)
        ((variable? exp)
         (lookup-variable-value exp env))

        ((not (null? (find-tag (car exp))))
         ((find-tag (car exp)) exp env))

        ((application? exp)
         (my-apply (my-eval (operator exp) env)
                (list-of-values
                 (operands exp)
                 env)))
        (else
         (error "Unknown expression
                 type: EVAL" exp))))

(define clause-dispatch
  (let ((clauses '(())))
    (define (find-tag-helper tag env)
      (cond ((null? env) nil)
            ((eq? (caar env) tag) (cdar env))
            (else (find-tag-helper tag (cdr env)))))

    (define (find-tag tag)
      (find-tag-helper tag (cdr clauses)))

    (define (set-tag tag func)
      (set-cdr! clauses (cons (cons tag func)
                              (cdr clauses))))

    (cons set-tag find-tag)))

(define (set-tag tag func)
  ((car clause-dispatch) tag func))

(define (find-tag tag)
  ((cdr clause-dispatch) tag))

(define (install-from tag function)
  (set-tag tag function))


;; clauses

(define (quotation args env)
  (text-of-quotation args))

(define (assignment args env)
  (eval-assignment args))

(define (definition args env)
  (eval-definition args env))

(define (if-clause args env)
  (eval-if args))

(define (lambda-clause exp env)
  (make-procedure
   (lambda-parameters exp)
   (lambda-body exp)
   env))

(define (begin-block exp env)
  (eval-sequence
   (begin-actions exp)
   env))

(define (cond-clause exp env)
  (my-eval (cond->if exp) env))


(install-from 'quote quotation)
(install-from 'set! assignment)
(install-from 'define definition)
(install-from 'if if-clause)
(install-from 'lambda lambda-clause)
(install-from 'begin begin-block)
(install-from 'cond cond-clause)


;; solution

(define (and-form exp env)
  (define (and-helper exp env)
    (cond ((null? exp) 'true)
          ((false? (my-eval (car exp) env))
           'false)
          (else
           (and-helper (cdr exp) env))))
  (and-helper (cdr exp) env))

(install-from 'and and-form)


(define (or-form exp env)
  (define (or-helper exp env)
    (cond ((null? exp) 'false)
          ((true? (my-eval (car exp) env))
           'true)
          (else
           (or-helper (cdr exp) env))))
  (or-helper (cdr exp) env))

(install-from 'or or-form)

;; test
(define (test)
  (define (test-eval exp)
    (my-eval exp the-global-environment))
  (test-equal (test-eval '(and (= 1 1) (= 1 0))) 'false)
  (test-equal (test-eval '(and (= 1 1) (= 1 1))) 'true)

  (test-equal (test-eval '(or (= 2 0) (= 1 0))) 'false)
  (test-equal (test-eval '(or (= 1 1) (= 1 0))) 'true))
