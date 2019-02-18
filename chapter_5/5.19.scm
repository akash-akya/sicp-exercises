#lang sicp
(#%require "utils.scm")

;; Exercise 5.19: Alyssa P. Hacker wants a breakpoint feature in the
;; simulator to help her debug her machine designs. You have been
;; hired to install this feature for her. She wants to be able to
;; specify a place in the controller sequence where the simulator will
;; stop and allow her to examine the state of the machine. You are to
;; implement a procedure

;; (set-breakpoint ⟨machine⟩ ⟨label⟩ ⟨n⟩)

;; that sets a breakpoint just before the n th instruction after the
;; given label. For example,

;; (set-breakpoint gcd-machine 'test-b 4)

;; installs a breakpoint in gcd-machine just before the assignment to
;; register a. When the simulator reaches the breakpoint it should
;; print the label and the offset of the breakpoint and stop executing
;; instructions. Alyssa can then use get-register-contents and
;; set-register-contents! to manipulate the state of the simulated
;; machine. She should then be able to continue execution by saying

;; (proceed-machine ⟨machine⟩)

;; She should also be able to remove a specific breakpoint by means of

;; (cancel-breakpoint ⟨machine⟩ ⟨label⟩ ⟨n⟩)

;; or to remove all breakpoints by means of

;; (cancel-all-breakpoints ⟨machine⟩)




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
  (let ((contents '*unassigned*)
        (trace 'off))
    (define (print-register-value value)
      (display "Name: ") (display name)
      (display " Old: ") (display contents)
      (display " New: ") (display value)
      (newline))
    (define (dispatch message)
      (cond ((eq? message 'get) contents)
            ((eq? message 'trace-on)
             (set! trace 'on))
            ((eq? message 'trace-off)
             (set! trace 'off))
            ((eq? message 'set)
             (lambda (value)
               (if (eq? trace 'on)
                   (print-register-value value))
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

(define (set-register-trace-on! register)
  (register 'trace-on))

(define (set-register-trace-off! register)
  (register 'trace-off))

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
        (instruction-exec-count 0)
        (trace 'off)
        (last-label nil)
        (breakpoints '())
        (offset 1)
        (the-instruction-sequence '()))
    (let ((the-ops
           (list
            (list 'initialize-stack
                  (lambda ()
                    (stack 'initialize)))
            (list 'print-instruction-count
                  (lambda ()
                    (newline)
                    (display (list 'instruction-count '= instruction-exec-count))))
            (list 'reset-instruction-count
                  (lambda ()
                    (set! instruction-exec-count 0)))
            (list 'trace-on
                  (lambda ()
                    (set! trace 'on)))
            (list 'trace-off
                  (lambda ()
                    (set! trace 'off)))))
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
      (define (print-instruction inst)
        (newline)
        (display (list last-label ': (instruction-text inst))))

      (define (execute)
        (define (execute-instruction inst)
          (set! offset (+ 1 offset))
          (if (eq? trace 'on) (print-instruction inst))
          ((instruction-execution-proc inst))
          (set! instruction-exec-count (+ 1 instruction-exec-count))
          (execute))

        (define (set-last-label inst)
          (set! last-label (label inst))
          (set! offset 1)
          ((instruction-execution-proc inst))
          (execute))

        (define (print-breakpoint label offset inst)
          (display (list 'breakpoint label offset (instruction-text inst)))
          (newline)
          (set! offset (+ 1 offset))
          (advance-pc pc))

        (let ((insts (get-contents pc)))
          ;; (display (list 'get-offset offset)) (newline)
          (cond ((null? insts) 'done)
                ((label-instruction? (car insts))
                 (set-last-label (car insts)))
                ((have-breakpoint? (make-breakpoint last-label offset) breakpoints)
                 (print-breakpoint last-label offset (car insts)))
                (else
                 (execute-instruction (car insts))))))

      (define (proceed)
        (set! offset (+ 1 offset))
        (execute))

      (set! the-ops
        (append
         the-ops
         (list
          (list 'register-trace-on
                (lambda (reg)
                  (set-register-trace-on! (lookup-register reg))))
          (list 'register-trace-off
                (lambda (reg)
                  (set-register-trace-off! (lookup-register reg)))))))

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
              ((eq? message 'set-breakpoint)
               (lambda (label line)
                 (set! breakpoints
                   (add-breakpoint (make-breakpoint label line) breakpoints))))
              ((eq? message 'remove-breakpoint)
               (lambda (label line)
                 (set! breakpoints
                   (remove-breakpoint (make-breakpoint label line) breakpoints))))
              ((eq? message 'remove-all-breakpoints)
               (lambda ()
                 (set! breakpoints '())))
              ((eq? message 'proceed)
               (proceed))
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

(define (make-breakpoint label line)
  (cons label line))

;; (define (breakpoint-label breakpoint)
;;   (car breakpoint))

;; (define (breakpoint-line breakpoint)
;;   (cadr breakpoint))

(define (add-breakpoint breakpoint breakpoints)
  (cons breakpoint breakpoints))

(define (have-breakpoint? breakpoint breakpoints)
  ;; (display (list 'have breakpoint)) (newline)
  (define (find bps)
    (cond ((null? bps) #f)
          ((equal? (car bps) breakpoint) #t)
          (else (find (cdr bps)))))
  (find breakpoints))

(define (remove-breakpoint breakpoint breakpoints)
  (define (remove bps)
    (cond ((null? bps) '())
          ((equal? (car bps) breakpoint) (cdr bps))
          (else (cons (car bps) (remove (cdr bps))))))
  (remove breakpoints))

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
               (let ((insts (cons (make-label-instruction
                                   next-inst)
                                  insts)))
                 (receive
                     insts
                     (cons (make-label-entry
                            next-inst
                            insts)
                      labels)))
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


(define (make-label-instruction text)
  (make-instruction (list 'label-start text)))
(define (label-instruction? inst)
  (eq? (car (instruction-text inst)) 'label-start))
(define (label inst)
  (cadr (instruction-text inst)))

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
        ((eq? (car inst) 'label-start)
         (lambda ()
           (advance-pc pc)))
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

(define (tagged-list? pair sym)
  (eq? (car pair) sym))

(define (set-breakpoint machine label line)
  ((machine 'set-breakpoint) label line))

(define (cancel-breakpoint machine label line)
  ((machine 'remove-breakpoint) label line))

(define (cancel-all-breakpoint machine)
  ((machine 'remove-all-breakpoints)))

(define (proceed-machine machine)
  (machine 'proceed))

;; test

(define (test-breakpoints)
  (let ((bps '()))
    (set! bps (add-breakpoint (make-breakpoint 'label-a 10) bps))
    (set! bps (add-breakpoint (make-breakpoint 'label-b 20) bps))

    (test-equal (have-breakpoint? 'something 1  bps) #f)
    (test-equal (have-breakpoint? 'label-a   1 bps) #f)
    (test-equal (have-breakpoint? 'label-c   10 bps) #f)
    (test-equal (have-breakpoint? 'label-a   10 bps) #t)

    (set! bps (remove-breakpoint 'label-a 10 bps))
    (test-equal (have-breakpoint? 'label-a   10 bps) #f)
    (test-equal (have-breakpoint? 'label-b   20 bps) #t)))


(define gcd-machine
  (make-machine
   '(a b t)
   (list (list 'rem remainder) (list '= =))
   '(test-b
     (test (op =) (reg b) (const 0))
     (branch (label gcd-done))
     (assign t (op rem) (reg a) (reg b))
     (assign a (reg b))
     (assign b (reg t))
     (goto (label test-b))
     gcd-done)))


(define (test)
  (set-register-contents! gcd-machine 'a 206)
  (set-register-contents! gcd-machine 'b 40)
  (set-breakpoint gcd-machine 'test-b 4)
  (set-breakpoint gcd-machine 'test-b 5)
  (start gcd-machine)

  (proceed-machine gcd-machine)
  (proceed-machine gcd-machine)


  (cancel-breakpoint gcd-machine 'test-b 4)
  (proceed-machine gcd-machine)

  (cancel-all-breakpoint gcd-machine)
  (proceed-machine gcd-machine)


  (newline)
  (test-equal (get-register-contents gcd-machine 'a) 2))
