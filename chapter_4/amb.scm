#lang sicp
(#%require racket)

(#%provide amb)
(define amb-fail '*)

(define (initialize-amb-fail)
  (set! amb-fail
    (lambda ()
      (error "amb tree exhausted"))))

(initialize-amb-fail)

(define (set-amb-fail! x)
  (set! amb-fail x))

(define-syntax (amb stx)
  (syntax-case stx ()
    [(_ alts ...)
       #`(let ((+prev-amb-fail amb-fail))
           (call/cc
            (lambda (+sk)
              #,@(map (lambda (alt)
                        #`(call/cc
                           (lambda (+fk)
                             (set-amb-fail!
                              (lambda ()
                                (set-amb-fail! +prev-amb-fail)
                                (+fk 'fail)))
                             (+sk #,alt))))
                      (syntax->list #'(alts ...)))
              (+prev-amb-fail))))]))
