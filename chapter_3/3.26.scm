#lang sicp
(#%require "utils.scm")

;;  Exercise 3.26: To search a table as implemented above, one needs
;;  to scan through the list of records. This is basically the
;;  unordered list representation of 2.3.3. For large tables, it may
;;  be more efficient to structure the table in a different
;;  manner. Describe a table implementation where the (key, value)
;;  records are organized using a binary tree, assuming that keys can
;;  be ordered in some way (e.g., numerically or
;;  alphabetically). (Compare Exercise 2.66 of Chapter 2.)

(define (assoc key records)
  (cond ((null? records) false)
        ((equal? key (caar records))
         (car records))
        (else (assoc key (cdr records)))))

;; record
(define (mk-record k d) (cons k d))

(define (key record) (car record))

(define (data record) (cdr record))

(define (set-data! record value) (set-cdr! record value))
;; tree
(define (mk-node record left right) (list record left right))

(define (left tree) (cadr tree))

(define (set-left! tree x) (set-car! (cdr tree) x))

(define (right tree) (caddr tree))

(define (set-right! tree x) (set-car! (cddr tree) x))

(define (entry tree) (car tree))

(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup-helper given-key tree)
      (if (null? tree)
          false
          (let ((k (key (entry tree))))
            (cond ((equal? given-key k)
                   (data (entry tree)))
                  ((< given-key k)
                   (lookup-helper given-key (left tree)))
                  (else
                   (lookup-helper given-key (right tree)))))))

    (define (lookup key)
      (lookup-helper key (cdr local-table)))

    (define (add-child! tree direction x)
      (if (eq? direction 'left)
          (set-left! tree x)
          (set-right! tree x)))

    (define (insert-helper! x tree parent direction value)
      (cond ((null? tree) (add-child! parent direction (mk-node (mk-record x value) nil nil)))
            ((= x (key (entry tree))) (set-data! (entry tree) value))
            ((< x (key (entry tree)))
             (insert-helper! x (left tree) tree 'left value))
            ((> x (key (entry tree)))
             (insert-helper! x (right tree) tree 'right value))))

    (define (insert! k value)
      (let ((tree (cdr local-table)))
        (cond ((null? tree)
               (set-cdr! local-table (mk-node (mk-record k value) nil nil)))
              ((or (= k (key (entry tree))))
               (set-data! (entry tree) value))
              (else
               (insert-helper! k tree tree nil value))))
      'ok)

    (define (print-table)
      (display local-table)
      (newline))

    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            ((eq? m 'print) print-table)
            (else (error "Unknown operation:
                          TABLE" m))))
    dispatch))

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))
;; (define print (operation-table 'print))

;; test
(put 30 'A)
(test-equal (get 30) 'A)

(put 40 'B)
(test-equal (get 40) 'B)

(put 10 'Z)
(test-equal (get 10) 'Z)

(put 25 'D)
(test-equal (get 25) 'D)
