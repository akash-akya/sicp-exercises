#lang sicp
(#%require "utils.scm")

;; Exercise 2.66: Implement the lookup procedure for the case where
;; the set of records is structured as a binary tree, ordered by the
;; numerical values of the keys.

;; record
(define (mk-record k d) (cons k d))

(define (key record) (car record))

(define (data record) (cdr record))

;; tree
(define (mk-node record left right) (list record left right))

(define (left tree) (cadr tree))

(define (right tree) (caddr tree))

(define (entry tree) (car tree))

;; lookup
(define (lookup given-key set-of-records)
  (if (null? set-of-records)
      false
      (let ((k (key (entry set-of-records))))
        (cond ((equal? given-key k)
               (data (entry set-of-records)))
              ((< given-key k)
               (lookup given-key (left set-of-records)))
              (else
               (lookup given-key (right set-of-records)))))))

;; test
(define t (mk-node (mk-record 5 "5")
                   (mk-node (mk-record 3 "3")
                            (mk-node (mk-record 2 "2") nil nil)
                            (mk-node (mk-record 4 "4") nil nil))
                   (mk-node (mk-record 7 "7")
                            (mk-node (mk-record 6 "6") nil nil)
                            (mk-node (mk-record 8 "8") nil nil))))
(test-equal (lookup 6 t) "6")

(test-equal (lookup 2 t) "2")
