#lang sicp
(#%require "utils.scm")

;; Exercise 4.13: Scheme allows us to create new bindings for
;; variables by means of define, but provides no way to get rid of
;; bindings. Implement for the evaluator a special form make-unbound!
;; that removes the binding of a given symbol from the environment in
;; which the make-unbound! expression is evaluated. This problem is
;; not completely specified. For example, should we remove only the
;; binding in the first frame of the environment? Complete the
;; specification and justify any choices you make.


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

;; ==== solution start ====
(define (traverse-frame frame var base-func func)
  (define (scan vars vals)
    (cond ((null? vars)
           (base-func frame))
          ((eq? var (car vars))
           (func vars vals))
          (else (scan (cdr vars) (cdr vals)))))
  (scan (frame-variables frame)
        (frame-values frame)))

(define (traverse-first-frame env var base-func func)
  (if (eq? env the-empty-environment)
      (error "Unbound variable" var)
      (traverse-frame (first-frame env) var base-func func)))

(define (lookup-variable-value var env)
  (traverse-first-frame env
            var
            (lambda (frame) (lookup-variable-value var (enclosing-environment env)))
            (lambda (vars vals)
              (car vals))))

(define (set-variable-value! var val env)
  (traverse-first-frame env
            var
            (lambda (frame) (set-variable-value! var val (enclosing-environment env)))
            (lambda (vars vals)
              (set-car! vals val))))

(define (define-variable! var val env)
  (traverse-first-frame env
            var
            (lambda (frame) (add-binding-to-frame! var val frame))
            (lambda (vars vals)
              (set-car! vals val))))

(define (pop list)
  (if (null? (cdr list))
      (begin
        (set-car! list nil))
      (let ((first (cadr list))
            (rest (cddr list)))
        (set-car! list first)
        (set-cdr! list rest))))

(define (undefine-variable! var env)
  (traverse-first-frame
   env
   var
   (lambda (frame) (error "Unbound variable" var))
   (lambda (vars vals)
     (pop vars)
     (pop vals)
     nil
     )))

;; ==== solution end ====

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
  (eval-assignment args env))

(define (definition args env)
  (eval-definition args env))

(define (if-clause args env)
  (eval-if args env))

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

(define (undefine-clause exp env)
  ;; (display exp) (newline)
  (undefine-variable! (cadr exp) env))

(install-from 'undefine! undefine-clause)

(define (log x)
  (display x)
  (newline))


;; test
(define (test)
  (define (test-eval exp)
    (my-eval exp the-global-environment))

  (define test-func '(begin (define a 10)
                            (define (dummy)
                              (define a 20)
                              (if (= a 20)
                                  (begin
                                    (undefine! a)
                                    (= a 10))
                                  #f))
                            (dummy)))
  (test-equal (test-eval test-func) #t))
