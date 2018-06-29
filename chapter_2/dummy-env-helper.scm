#lang sicp

;; Dummy implementation of the env
(define *env* nil)

(define (get-index entry) (car entry))
;; (define (get-type entry) (cadr entry))
(define (get-item entry) (cdr entry))

(define (print-error arg)
  (display "Passed arg")
  (display arg)
  (newline)
  (error "Not in env!"))

(define (get op type)
  (let ((index (cons op type)))
   (define (iter lst)
    (cond ((null? lst) (print-error (list op type)))
          ((equal? index (get-index (car lst)))
           (get-item (car lst)))
          (else (iter (cdr list)))))
   (iter *env*)))

(define (put op type item)
  (set! *env* (cons (cons (cons op type) item) *env*)))

(#%provide get)
(#%provide put)
