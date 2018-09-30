#lang sicp
(#%require "utils.scm")

;; Exercise 4.31: The approach taken in this section is somewhat
;; unpleasant, because it makes an incompatible change to Scheme. It
;; might be nicer to implement lazy evaluation as an upward-compatible
;; extension, that is, so that ordinary Scheme programs will work as
;; before. We can do this by extending the syntax of procedure
;; declarations to let the user control whether or not arguments are
;; to be delayed. While we’re at it, we may as well also give the user
;; the choice between delaying with and without memoization. For
;; example, the definition

;; (define (f a (b lazy) c (d lazy-memo))
;;   …)

;; would define f to be a procedure of four arguments, where the first
;; and third arguments are evaluated when the procedure is called, the
;; second argument is delayed, and the fourth argument is both delayed
;; and memoized. Thus, ordinary procedure definitions will produce the
;; same behavior as ordinary Scheme, while adding the lazy-memo
;; declaration to each parameter of every compound procedure will
;; produce the behavior of the lazy evaluator defined in this
;; section. Design and implement the changes required to produce such
;; an extension to Scheme. You will have to implement new syntax
;; procedures to handle the new syntax for define. You must also
;; arrange for eval or apply to determine when arguments are to be
;; delayed, and to force or delay arguments accordingly, and you must
;; arrange for forcing to memoize or not, as appropriate.

(define (actual-value exp env)
  (force-it (my-eval exp env)))

(define (my-apply procedure arguments env)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure
          procedure
          (list-of-arg-values
           arguments
           env)))  ; changed
        ((compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (extend-environment
           (arg-names (procedure-parameters procedure))
           (list-of-args
            (procedure-parameters procedure)
            arguments
            env)   ; changed
           (procedure-environment procedure))))
        (else (error "Unknown procedure
                      type: APPLY"
                     procedure))))

(define (arg-names params)
  (cond ((null? params) nil)
        ((symbol? (car params))
         (cons (car params) (arg-names (cdr params))))
        (else
         (cons (caar params) (arg-names (cdr params))))))

(define (list-of-args params exps env)
  (if (no-operands? exps)
      '()
      (cons
       (let ((p (first-operand params))
             (exp (first-operand exps)))
         (if (lazy? p)
             (delay-it exp env (lazy-type p))
             (actual-value exp env)))
       (list-of-args
        (rest-operands params)
        (rest-operands exps)
        env))))

(define (lazy-type exp)
  (last exp))

(define (lazy? exp)
  (not
   (false?
    (and
     (list? exp)
     (member (last exp) '(lazy lazy-memo))))))

(define (last exp)
  (cond ((null? exp) nil)
        ((= (length exp) 1) (car exp))
        (else (last (cdr exp)))))

(define (list-of-arg-values exps env)
  (if (no-operands? exps)
      '()
      (cons (actual-value
             (first-operand exps)
             env)
            (list-of-arg-values
             (rest-operands exps)
             env))))

;; (define (list-of-delayed-args exps env)
;;   (if (no-operands? exps)
;;       '()
;;       (cons (delay-it
;;              (first-operand exps)
;;              env)
;;             (list-of-delayed-args
;;              (rest-operands exps)
;;              env))))

(define (eval-if exp env)
  (if (true? (actual-value (if-predicate exp)
                           env))
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
        (list '= =)
        (list '* *)))

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

(define (delay-it exp env mem)
  (list 'thunk exp env mem))
(define (thunk? obj) (tagged-list? obj 'thunk))
(define (thunk-exp thunk) (cadr thunk))
(define (thunk-env thunk) (caddr thunk))
(define (with-memo? thunk) (eq? (cadddr thunk) 'lazy-memo))

(define (evaluated-thunk? obj)
  (tagged-list? obj 'evaluated-thunk))

(define (thunk-value evaluated-thunk)
  (cadr evaluated-thunk))

(define input-prompt  ";;; L-Eval input:")
(define output-prompt ";;; L-Eval value:")

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (actual-value
                   input
                   the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))

(define (prompt-for-input string)
  (newline) (newline)
  (display string) (newline))

(define (announce-output string)
  (newline) (display string) (newline))

(define (user-print object)
  (if (compound-procedure? object)
      (display
       (list 'compound-procedure
             (procedure-parameters object)
             (procedure-body object)
             '<procedure-env>))
      (display object)))



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
         ;; (display (operands exp)) (newline)
         (my-apply (actual-value (operator exp) env)
                   (operands exp)
                   env))
        (else
         (error "Unknown expression
                 type: EVAL" exp))))


(define (set-object! obj result)
  (set-car! obj 'evaluated-thunk)
  ;; replace exp with its value:
  (set-car! (cdr obj) result)
  ;; forget unneeded env:
  (set-cdr! (cdr obj) '()))

;;; with memoization
(define (force-it obj)
  (cond ((thunk? obj)
         (let ((result
                (actual-value
                 (thunk-exp obj)
                 (thunk-env obj))))
           (if (with-memo? obj)
               (set-object! obj result))
           result))
        ((evaluated-thunk? obj)
         (thunk-value obj))
        (else obj)))


;; test
;; (define (test)
;;   (actual-value '(begin
;;                    (define (a x)
;;                      (x))
;;                    (a (lambda () (+ 1 1))))
;;                 the-global-environment))

(define (test)
  (define the-global-environment
    (setup-environment))

  (define (test-eval exp)
    (my-eval exp the-global-environment))

  (test-equal
      (test-eval
       '(begin
          (define n 0)
          (define (a (x lazy))
            (if 'true
                10
                (x)))
          (a (set! n 1))
          n))
    0)

  (test-equal
      (test-eval
       '(begin
          (define n 0)
          (define (a x)
            (if 'true
                10
                (x)))
          (a (set! n 1))
          n))
    1))
