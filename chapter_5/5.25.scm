#lang sicp

;;  Exercise 5.24: Implement cond as a new basic special form without
;;     reducing it to if. You will have to construct a loop that tests
;;     the predicates of successive cond clauses until you find one
;;     that is true, and then use ev-sequence to evaluate the actions
;;     of the clause.



(#%require "utils.scm")
(#%require rackunit)
(#%require rackunit/text-ui)

;; Machine
(define (make-machine register-names
                      ops
                      controller-text)
  (let ((machine (make-new-machine)))
    (for-each (lambda (register-name)
                ((machine 'allocate-register)
                 register-name))
              register-names)
    ((machine 'install-operations) ops)
    ((machine 'install-instruction-sequence)
     (assemble controller-text machine))
    machine))

;; Registers
(define (make-register name)
  (let ((contents '*unassigned*))
    (define (dispatch message)
      (cond ((eq? message 'get) contents)
            ((eq? message 'set)
             (lambda (value)
               (set! contents value)))
            (else
             (error "Unknown request:
                     REGISTER"
                    message))))
    dispatch))

(define (get-contents register)
  (register 'get))

(define (set-contents! register value)
  ((register 'set) value))

;; Stack
(define (make-stack)
  (let ((s '()))
    (define (push x)
      (set! s (cons x s)))
    (define (pop)
      (if (null? s)
          (error "Empty stack: POP")
          (let ((top (car s)))
            (set! s (cdr s))
            top)))
    (define (initialize)
      (set! s '())
      'done)
    (define (dispatch message)
      (cond ((eq? message 'push) push)
            ((eq? message 'pop) (pop))
            ((eq? message 'initialize)
             (initialize))
            (else
             (error "Unknown request: STACK"
                    message))))
    dispatch))
(define (pop stack) (stack 'pop))
(define (push stack value)
  ((stack 'push) value))

;; The basic machine
(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
        (the-instruction-sequence '()))
    (let ((the-ops
           (list
            (list 'initialize-stack
                  (lambda ()
                    (stack 'initialize)))))
          (register-table
           (list (list 'pc pc)
                 (list 'flag flag))))
      (define (allocate-register name)
        (if (assoc name register-table)
            (error
             "Multiply defined register: "
             name)
            (set! register-table
              (cons
               (list name
                     (make-register name))
               register-table)))
        'register-allocated)
      (define (lookup-register name)
        (let ((val
               (assoc name register-table)))
          (if val
              (cadr val)
              (error "Unknown register:"
                     name))))
      (define (execute)
        (let ((insts (get-contents pc)))
          (if (null? insts)
              'done
              (begin
                ;; (if (pair? insts)
                ;;     (if (pair? (car insts))
                ;;         (display (caar insts))
                ;;         (display (car insts)))
                ;;     (display insts))
                ;; (newline)
                ((instruction-execution-proc
                  (car insts)))
                (execute)))))
      (define (dispatch message)
        (cond ((eq? message 'start)
               (set-contents!
                pc
                the-instruction-sequence)
               (execute))
              ((eq?
                message
                'install-instruction-sequence)
               (lambda (seq)
                 (set!
                     the-instruction-sequence
                   seq)))
              ((eq? message
                    'allocate-register)
               allocate-register)
              ((eq? message 'get-register)
               lookup-register)
              ((eq? message
                    'install-operations)
               (lambda (ops)
                 (set! the-ops
                   (append the-ops ops))))
              ((eq? message 'stack) stack)
              ((eq? message 'operations)
               the-ops)
              (else (error "Unknown request:
                            MACHINE"
                           message))))
      dispatch)))

(define (start machine)
  (machine 'start))

(define (get-register-contents
         machine register-name)
  (get-contents
   (get-register machine register-name)))

(define (set-register-contents!
         machine register-name value)
  (set-contents!
   (get-register machine register-name)
   value)
  'done)

(define (get-register machine reg-name)
  ((machine 'get-register) reg-name))

;; The assembler
(define (assemble controller-text machine)
  (extract-labels controller-text
                  (lambda (insts labels)
                    (update-insts! insts labels machine)
                    insts)))
(define (extract-labels text receive)
  (if (null? text)
      (receive '() '())
      (extract-labels
       (cdr text)
       (lambda (insts labels)
         (let ((next-inst (car text)))
           (if (symbol? next-inst)
               (receive
                   insts
                   (cons
                    (make-label-entry
                     next-inst
                     insts)
                    labels))
               (receive
                   (cons (make-instruction
                          next-inst)
                         insts)
                   labels)))))))

(define (update-insts! insts labels machine)
  (let ((pc (get-register machine 'pc))
        (flag (get-register machine 'flag))
        (stack (machine 'stack))
        (ops (machine 'operations)))
    (for-each
     (lambda (inst)
       (set-instruction-execution-proc!
        inst
        (make-execution-procedure
         (instruction-text inst)
         labels
         machine
         pc
         flag
         stack
         ops)))
     insts)))

(define (make-instruction text)
  (cons text '()))
(define (instruction-text inst) (car inst))
(define (instruction-execution-proc inst)
  (cdr inst))
(define (set-instruction-execution-proc!
         inst
         proc)
  (set-cdr! inst proc))

(define (make-label-entry label-name insts)
  (cons label-name insts))

(define (lookup-label labels label-name)
  (let ((val (assoc label-name labels)))
    (if val
        (cdr val)
        (error "Undefined label: ASSEMBLE"
               label-name))))

;; Generating execution procedures for instructions
(define (make-execution-procedure
         inst labels machine pc flag stack ops)
  (cond ((eq? (car inst) 'assign)
         (make-assign
          inst machine labels ops pc))
        ((eq? (car inst) 'test)
         (make-test
          inst machine labels ops flag pc))
        ((eq? (car inst) 'branch)
         (make-branch
          inst machine labels flag pc))
        ((eq? (car inst) 'goto)
         (make-goto inst machine labels pc))
        ((eq? (car inst) 'save)
         (make-save inst machine stack pc))
        ((eq? (car inst) 'restore)
         (make-restore inst machine stack pc))
        ((eq? (car inst) 'perform)
         (make-perform
          inst machine labels ops pc))
        (else (error "Unknown instruction
                      type: ASSEMBLE"
                     inst))))

;; Assign instructions
(define (make-assign
         inst machine labels operations pc)
  (let ((target
         (get-register
          machine
          (assign-reg-name inst)))
        (value-exp (assign-value-exp inst)))
    (let ((value-proc
           (if (operation-exp? value-exp)
               (make-operation-exp
                value-exp
                machine
                labels
                operations)
               (make-primitive-exp
                (car value-exp)
                machine
                labels))))
      (lambda ()   ; execution procedure
                                        ; for assign
        (set-contents! target (value-proc))
        (advance-pc pc)))))

(define (assign-reg-name assign-instruction)
  (cadr assign-instruction))
(define (assign-value-exp assign-instruction)
  (cddr assign-instruction))

(define (advance-pc pc)
  (set-contents! pc (cdr (get-contents pc))))

;; Test, branch, and goto instructions
(define
  (make-test
   inst machine labels operations flag pc)
  (let ((condition (test-condition inst)))
    (if (operation-exp? condition)
        (let ((condition-proc
               (make-operation-exp
                condition
                machine
                labels
                operations)))
          (lambda ()
            (set-contents!
             flag (condition-proc))
            (advance-pc pc)))
        (error "Bad TEST instruction:
                ASSEMBLE" inst))))

(define (test-condition test-instruction)
  (cdr test-instruction))

(define
  (make-branch
   inst machine labels flag pc)
  (let ((dest (branch-dest inst)))
    (if (label-exp? dest)
        (let ((insts
               (lookup-label
                labels
                (label-exp-label dest))))
          (lambda ()
            (if (get-contents flag)
                (set-contents! pc insts)
                (advance-pc pc))))
        (error "Bad BRANCH instruction:
                ASSEMBLE"
               inst))))

(define (branch-dest branch-instruction)
  (cadr branch-instruction))

(define (make-goto inst machine labels pc)
  (let ((dest (goto-dest inst)))
    (cond ((label-exp? dest)
           (let ((insts
                  (lookup-label
                   labels
                   (label-exp-label dest))))
             (lambda ()
               (set-contents! pc insts))))
          ((register-exp? dest)
           (let ((reg
                  (get-register
                   machine
                   (register-exp-reg dest))))
             (lambda ()
               (set-contents!
                pc
                (get-contents reg)))))
          (else (error "Bad GOTO instruction:
                        ASSEMBLE"
                       inst)))))

(define (goto-dest goto-instruction)
  (cadr goto-instruction))

;; Other instructions
(define (make-save inst machine stack pc)
  (let ((reg (get-register
              machine
              (stack-inst-reg-name inst))))
    (lambda ()
      (push stack (get-contents reg))
      (advance-pc pc))))

(define (make-restore inst machine stack pc)
  (let ((reg (get-register
              machine
              (stack-inst-reg-name inst))))
    (lambda ()
      (set-contents! reg (pop stack))
      (advance-pc pc))))

(define (stack-inst-reg-name
         stack-instruction)
  (cadr stack-instruction))



(define (make-perform
         inst machine labels operations pc)
  (let ((action (perform-action inst)))
    (if (operation-exp? action)
        (let ((action-proc
               (make-operation-exp
                action
                machine
                labels
                operations)))
          (lambda ()
            (action-proc)
            (advance-pc pc)))
        (error "Bad PERFORM instruction:
                ASSEMBLE"
               inst))))

(define (perform-action inst) (cdr inst))

;; Execution procedures for subexpressions
(define (make-primitive-exp exp machine labels)
  (cond ((constant-exp? exp)
         (let ((c (constant-exp-value exp)))
           (lambda () c)))
        ((label-exp? exp)
         (let ((insts
                (lookup-label
                 labels
                 (label-exp-label exp))))
           (lambda () insts)))
        ((register-exp? exp)
         (let ((r (get-register
                   machine
                   (register-exp-reg exp))))
           (lambda () (get-contents r))))
        (else (error "Unknown expression type:
                      ASSEMBLE"
                     exp))))

(define (register-exp? exp)
  (tagged-list? exp 'reg))
(define (register-exp-reg exp)
  (cadr exp))
(define (constant-exp? exp)
  (tagged-list? exp 'const))
(define (constant-exp-value exp)
  (cadr exp))
(define (label-exp? exp)
  (tagged-list? exp 'label))
(define (label-exp-label exp)
  (cadr exp))

(define (make-operation-exp
         exp machine labels operations)
  (let ((op (lookup-prim
             (operation-exp-op exp)
             operations))
        (aprocs
         (map (lambda (e)
                (make-primitive-exp
                 e machine labels))
              (operation-exp-operands exp))))
    (lambda () (apply op (map (lambda (p) (p))
                              aprocs)))))

(define (operation-exp? exp)
  (and (pair? exp)
       (tagged-list? (car exp) 'op)))
(define (operation-exp-op operation-exp)
  (cadr (car operation-exp)))
(define (operation-exp-operands operation-exp)
  (cdr operation-exp))

(define (lookup-prim symbol operations)
  (let ((val (assoc symbol operations)))
    (if val
        (cadr val)
        (error "Unknown operation: ASSEMBLE"
               symbol))))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

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

(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

(define (variable? exp) (symbol? exp))

(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (text-of-quotation exp)
  (cadr exp))

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

(define (true? x)
  (not (eq? x false)))

(define (false? x)
  (eq? x false))

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc)
  (cadr proc))


(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'set-car! set-car!)
        (list 'set-cdr! set-cdr!)
        (list 'cons cons)
        (list 'null? null?)
        (list 'eq? eq?)
        (list '/ /)
        (list '=  (lambda (x y) (if (= x y) 'true 'false)))
        (list '+ +)
        (list '- -)
        (list '* *)
        (list 'display display)))

(define (primitive-procedure-names)
  (map car primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc)
         (list 'primitive (cadr proc)))
       primitive-procedures))

(define apply-in-underlying-scheme apply)

(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme
   (primitive-implementation proc) args))

(define (empty-arglist) '())
(define (adjoin-arg arg arglist)
  (append arglist (list arg)))

(define (last-operand? ops) (null? (cdr ops)))

(define (user-print object)
  (cond ((not (pair? object)) (display object) (newline))
        ((compound-procedure? object)
         (display
          (list 'compound-procedure
                (procedure-parameters object)
                (procedure-body object)
                '<procedure-env>)))
        (else (display object))))


;; (define input-prompt  ";;; M-Eval input:")
;; (define output-prompt ";;; M-Eval value:")

;; (define (driver-loop)
;;   (prompt-for-input input-prompt)
;;   (let ((input (read)))
;;     (let ((output
;;            (eval input
;;                  the-global-environment)))
;;       (announce-output output-prompt)
;;       (user-print output)))
;;   (driver-loop))

(define (prompt-for-input string)
  (newline) (display string) (newline))

(define (announce-output string)
  (display string))

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

(define (get-global-environment)
  the-global-environment)

;;cond
(define (cond? exp) (tagged-list? exp 'cond))

(define (cond-clauses exp) (cdr exp))

(define (cond-actions exp) (cdr exp))

(define (cond-predicate exp) (car exp))

(define (empty-clause? exp) (null? exp))

(define (else? exp)
  (eq? exp 'else))

;; normal order
(define (delay-it exp env)
  (list 'thunk exp env))
(define (thunk? obj) (tagged-list? obj 'thunk))
(define (thunk-exp thunk) (cadr thunk))
(define (thunk-env thunk) (caddr thunk))

(define (evaluated-thunk? obj)
  (tagged-list? obj 'evaluated-thunk))

(define (thunk-value evaluated-thunk)
  (cadr evaluated-thunk))

(define (set-evaluated obj result)
  (set-car! obj 'evaluated-thunk)
  ;; replace exp with its value:
  (set-car! (cdr obj) result)
  ;; forget unneeded env:
  (set-cdr! (cdr obj) '())
  result)


(define (list-of-delayed-args exps env)
  (if (no-operands? exps)
      '()
      (cons (delay-it
             (first-operand exps)
             env)
            (list-of-delayed-args
             (rest-operands exps)
             env))))

(define eceval
  (make-machine
   '(exp env val continue proc argl unev)
   (list
    (list 'make-procedure make-procedure)
    (list 'compound-procedure? compound-procedure?)
    (list 'procedure-parameters procedure-parameters)
    (list 'procedure-body procedure-body)
    (list 'procedure-environment procedure-environment)
    (list 'enclosing-environment enclosing-environment)
    (list 'first-frame first-frame)
    (list 'make-frame make-frame)
    (list 'frame-variables frame-variables)
    (list 'frame-values frame-values)
    (list 'add-binding-to-frame! add-binding-to-frame!)
    (list 'extend-environment extend-environment)
    (list 'lookup-variable-value lookup-variable-value)
    (list 'make-procedure make-procedure)
    (list 'lookup-variable-value lookup-variable-value)
    (list 'self-evaluating? self-evaluating?)
    (list 'variable? variable?)
    (list 'quoted? quoted?)
    (list 'text-of-quotation text-of-quotation)
    (list 'assignment? assignment?)
    (list 'assignment-variable assignment-variable)
    (list 'assignment-value assignment-value)
    (list 'definition? definition?)
    (list 'definition-variable definition-variable)
    (list 'definition-value definition-value)
    (list 'lambda? lambda?)
    (list 'lambda-parameters lambda-parameters)
    (list 'lambda-body lambda-body)
    (list 'make-lambda make-lambda)
    (list 'if? if?)
    (list 'if-predicate if-predicate)
    (list 'if-consequent if-consequent)
    (list 'if-alternative if-alternative)
    (list 'begin? begin?)
    (list 'begin-actions begin-actions)
    (list 'last-exp? last-exp?)
    (list 'first-exp first-exp)
    (list 'rest-exps rest-exps)
    (list 'sequence->exp sequence->exp)
    (list 'make-begin make-begin)
    (list 'application? application?)
    (list 'operator operator)
    (list 'operands operands)
    (list 'no-operands? no-operands?)
    (list 'first-operand first-operand)
    (list 'rest-operands rest-operands)
    (list 'true? true?)
    (list 'false? false?)
    (list 'empty-arglist empty-arglist)
    (list 'adjoin-arg adjoin-arg)
    (list 'last-operand? last-operand?)
    (list 'primitive-procedure? primitive-procedure?)
    (list 'apply-primitive-procedure apply-primitive-procedure)
    (list 'set-variable-value! set-variable-value!)
    (list 'define-variable! define-variable!)
    (list 'announce-output announce-output)
    (list 'user-print user-print)
    (list 'prompt-for-input prompt-for-input)
    (list 'read read)
    (list 'get-global-environment get-global-environment)
    (list 'cond? cond?)
    (list 'cond-actions cond-actions)
    (list 'cond-clauses cond-clauses)
    (list 'cond-predicate cond-predicate)
    (list 'empty-clause? empty-clause?)
    (list 'else? else?)
    (list 'display (lambda (x) (display x) (newline)))

    (list 'delay-it delay-it)
    (list 'thunk? thunk?)
    (list 'thunk-exp thunk-exp)
    (list 'thunk-env thunk-env)
    (list 'evaluated-thunk? evaluated-thunk?)
    (list 'thunk-value thunk-value)
    (list 'set-evaluated set-evaluated)
    (list 'list-of-delayed-args list-of-delayed-args)
    (list 'reverse reverse))

   '(read-eval-print-loop
     (perform (op initialize-stack))
     (perform (op prompt-for-input)
              (const ";;; EC-Eval input:"))
     (assign exp (op read))
     (assign env (op get-global-environment))
     (assign continue (label print-result))
     (goto (label eval-dispatch))

     eval-dispatch
     ;; (perform (op display) (reg exp))
     (test (op self-evaluating?) (reg exp))
     (branch (label ev-self-eval))
     (test (op variable?) (reg exp))
     (branch (label ev-variable))
     (test (op quoted?) (reg exp))
     (branch (label ev-quoted))
     (test (op assignment?) (reg exp))
     (branch (label ev-assignment))
     (test (op definition?) (reg exp))
     (branch (label ev-definition))
     (test (op if?) (reg exp))
     (branch (label ev-if))
     (test (op lambda?) (reg exp))
     (branch (label ev-lambda))
     (test (op cond?) (reg exp))
     (branch (label ev-cond))
     (test (op begin?) (reg exp))
     (branch (label ev-begin))
     (test (op application?) (reg exp))
     (branch (label ev-application))
     (goto (label unknown-expression-type))

     ev-self-eval
     (assign val (reg exp))
     (goto (reg continue))
     ev-variable
     (assign val
             (op lookup-variable-value)
             (reg exp)
             (reg env))
     (goto (reg continue))
     ev-quoted
     (assign val
             (op text-of-quotation)
             (reg exp))
     (goto (reg continue))
     ev-lambda
     (assign unev
             (op lambda-parameters)
             (reg exp))
     (assign exp
             (op lambda-body)
             (reg exp))
     (assign val
             (op make-procedure)
             (reg unev)
             (reg exp)
             (reg env))
     (goto (reg continue))



     ;;;
     ;;;  actual-value
     ;;;
     actual-value
     (save exp)
     (save env)
     (save continue)
     (assign continue (label after-actual-value-eval))
     (goto (label eval-dispatch))

     after-actual-value-eval
     (assign exp (reg val))
     (assign continue (label after-actual-value))
     (goto (label force-it))

     after-actual-value
     (restore continue)
     (restore env)
     (restore exp)
     (goto (reg continue))


     ;;;
     ;;;  force-it
     ;;;
     force-it
     (save continue)
     (assign continue (label after-force-it))
     (test (op thunk?) (reg exp))
     (branch (label force-thunk))
     (test (op evaluated-thunk?) (reg exp))
     (branch (label thunk-value))
     (assign val (reg exp))
     (goto (label after-force-it))


     force-thunk
     (save exp)
     (assign env (op thunk-env) (reg exp))
     (assign exp (op thunk-exp) (reg exp))
     (save continue)
     (assign continue (label after-force-thunk))
     (goto (label actual-value))

     after-force-thunk
     (restore continue)
     (restore exp)
     (assign val (op set-evaluated)
             (reg exp)
             (reg val))
     (goto (reg continue))


     thunk-value
     (assign val (op thunk-value) (reg exp))
     (goto (reg continue))


     after-force-it
     (restore continue)
     (goto (reg continue))


     ;; apply

     ev-application
     (save continue) ;; save what to-do after applying exp


     (save env)
     (save exp)

     (assign continue (label apply-after-actual-value))
     (assign exp (op operator) (reg exp))
     (goto (label actual-value))

     apply-after-actual-value
     (restore exp)
     (restore env)

     (assign argl (op operands) (reg exp))
     ;; (assign exp (reg val))
     (assign proc (reg val))    ; the operator

     (test (op primitive-procedure?) (reg proc))
     (branch (label list-of-arg-values))
     (test (op compound-procedure?) (reg proc))
     (branch (label compound-apply))
     (goto (label unknown-procedure-type))


     list-of-arg-values
     ;; (save exp)
     (save proc)
     (save env)
     (assign argl (op reverse) (reg argl))
     ;; (perform (op display) (reg argl))
     (assign unev (op empty-arglist))
     ;; (assign continue (label start-arg-values))

     start-arg-values
     ;; (perform (op display) (reg argl))
     ;; (perform (op display) (reg unev))

     (test (op no-operands?) (reg argl))
     (branch (label after-list-of-arg-values))
     (save unev)
     (save argl)
     (save env)
     (assign exp (op first-operand) (reg argl))
     (assign continue (label cons-arg-list))
     (goto (label actual-value))

     cons-arg-list
     (restore env)
     (restore argl)
     (restore unev)
     (assign unev (op make-frame) (reg val) (reg unev))
     (assign argl (op rest-operands) (reg argl))
     (assign continue (label start-arg-values))
     (goto (label start-arg-values))

     after-list-of-arg-values
     (restore env)
     (restore proc)
     ;; (restore exp)
     (assign argl (reg unev))
     (goto (label primitive-apply))

     primitive-apply
     (assign val (op apply-primitive-procedure)
             (reg proc)
             (reg argl))
     (restore continue)
     (goto (reg continue))


     compound-apply
     (assign unev
             (op procedure-parameters)
             (reg proc))
     (assign env
             (op procedure-environment)
             (reg proc))
     (assign argl
             (op list-of-delayed-args)
             (reg argl)
             (reg env))
     (assign env
             (op extend-environment)
             (reg unev)
             (reg argl)
             (reg env))
     (assign unev
             (op procedure-body)
             (reg proc))
     (goto (label ev-sequence))


     ev-begin
     (assign unev
             (op begin-actions)
             (reg exp))
     (save continue)
     (goto (label ev-sequence))


     ev-sequence
     (assign exp (op first-exp) (reg unev))
     (test (op last-exp?) (reg unev))
     (branch (label ev-sequence-last-exp))
     (save unev)
     (save env)
     (assign continue
             (label ev-sequence-continue))
     (goto (label eval-dispatch))
     ev-sequence-continue
     (restore env)
     (restore unev)
     (assign unev
             (op rest-exps)
             (reg unev))
     (goto (label ev-sequence))
     ev-sequence-last-exp
     (restore continue)
     (goto (label eval-dispatch))


     ev-if
     (save exp)   ; save expression for later
     (save env)
     (save continue)
     (assign exp (op if-predicate) (reg exp))
     (assign continue (label after-if))
     (goto (label actual-value))

     after-if
     (restore continue)
     (restore env)
     (restore exp)

     (save exp)   ; save expression for later
     (save env)
     (save continue)

     (assign continue (label ev-if-decide))
     (assign exp  (reg val))
     (goto (label eval-dispatch))

     ev-if-decide
     (restore continue)
     (restore env)
     (restore exp)
     (test (op true?) (reg val))
     (branch (label ev-if-consequent))
     ev-if-alternative
     (assign exp (op if-alternative) (reg exp))
     (goto (label eval-dispatch))
     ev-if-consequent
     (assign exp (op if-consequent) (reg exp))
     (goto (label eval-dispatch))

     ev-assignment
     (assign unev
             (op assignment-variable)
             (reg exp))
     (save unev)   ; save variable for later
     (assign exp
             (op assignment-value)
             (reg exp))
     (save env)
     (save continue)
     (assign continue
             (label ev-assignment-1))
                                        ; evaluate the assignment value:
     (goto (label eval-dispatch))
     ev-assignment-1
     (restore continue)
     (restore env)
     (restore unev)
     (perform (op set-variable-value!)
              (reg unev)
              (reg val)
              (reg env))
     (assign val
             (const ok))
     (goto (reg continue))

     ;; cond
     ev-cond
     (assign unev (op cond-clauses) (reg exp))
     (save continue)

     ev-cond-continue
     (test (op empty-clause?) (reg unev))
     (branch (label cond-complete))
     (assign exp (op first-exp) (reg unev))

     (save exp)
     (save env)
     (save unev)
     (assign exp (op cond-predicate) (reg exp))
     (test (op else?) (reg exp))
     (branch (label handle-else))

     (assign continue
             (label after-cond-predicate))
     (goto (label eval-dispatch))

     handle-else
     (restore unev)
     (restore env)
     (restore exp)
     (goto (label exec-cond-actions))

     after-cond-predicate
     (restore unev)
     (restore env)
     (restore exp)
     (test (op false?) (reg val))
     (branch (label cond-other-clause))

     exec-cond-actions
     ;; (assign exp (op first-exp) (reg unev))
     (assign unev (op cond-actions) (reg exp))
     ;; (perform (op display) (reg exp))
     (goto (label ev-sequence))

     cond-complete
     (restore continue)
     (goto (label eval-dispatch))

     cond-other-clause
     (assign unev (op rest-exps) (reg unev))
     (goto (label ev-cond-continue))

     ev-definition
     (assign unev
             (op definition-variable)
             (reg exp))
     (save unev)   ; save variable for later
     (assign exp
             (op definition-value)
             (reg exp))
     (save env)
     (save continue)
     (assign continue (label ev-definition-1))
                                        ; evaluate the definition value:
     (goto (label eval-dispatch))
     ev-definition-1
     (restore continue)
     (restore env)
     (restore unev)
     (perform (op define-variable!)
              (reg unev)
              (reg val)
              (reg env))
     (assign val (const ok))
     (goto (reg continue))

     print-result
     (perform (op announce-output)
              (const ";;; EC-Eval value:"))
     (perform (op user-print) (reg val))
     ;; (goto (label done))
     (goto (label read-eval-print-loop))

     unknown-expression-type
     (assign
      val
      (const unknown-expression-type-error))
     (goto (label signal-error))

     unknown-procedure-type  ; clean up stack (from apply-dispatch):
     (restore continue)
     (assign
      val
      (const unknown-procedure-type-error))
     (goto (label signal-error))
     signal-error
     (perform (op user-print) (reg val))
     (goto (label read-eval-print-loop))

     done
     )))


;; (set-register-contents! eceval 'exp '3)

;; (start eceval)

;;;; Expression
;; (cond (#f (display 10) ) (else (display 0)))


;; test
(define tests
  (test-suite
   "Machine tests"


   ;; (start lisp-machine)
   ;; (check-equal? (get-register-contents lisp-machine 'val) 3)
   ))

(run-tests tests)