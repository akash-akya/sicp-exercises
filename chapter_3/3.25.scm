#lang sicp
(#%require "utils.scm")

;;  Exercise 3.25: Generalizing one- and two-dimensional tables, show
;;  how to implement a table in which values are stored under an
;;  arbitrary number of keys and different values may be stored under
;;  different numbers of keys. The lookup and insert! procedures
;;  should take as input a list of keys used to access the table.


(define (assoc key records)
  (cond ((null? records) false)
        ((equal? key (caar records))
         (car records))
        (else (assoc key (cdr records)))))

(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key)
      (let ((record (assoc key (cdr local-table))))
        (if record
            (cdr record)
            false)))
    (define (insert! key value)
      (let ((record (assoc key (cdr local-table))))
        (if record
            (set-cdr! record value)
            (set-cdr! local-table
                      (cons (cons key value)
                            (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation:
                          TABLE" m))))
    dispatch))

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))


;; test
(put '(1 2 3) 300)
(test-equal (get '(1 2 3)) 300)

(put '(10 3) 1000)
(test-equal (get '(10 3)) 1000)
