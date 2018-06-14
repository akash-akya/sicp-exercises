#lang sicp

;; Exercise 2.59: Implement the union-set operation for the
;; unordered-list representation of sets.

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))


(define (union set1 set2)
  (if (null? set1)
      set2
      (union (cdr set1)
             (if (element-of-set? (car set1) set2)
                 set2
                 (cons (car set1) set2)))))


;; test
(define (assert exp result)
  (cond ((not (equal? exp result))
         (display "Failed!")
         (display exp)
         #t)
        (else #f)))

(define (test)
  (cond ((assert (union '(1 2 3 4 9) '(4 5 6))
                 '(9 3 2 1 4 5 6)))
        ((assert (union '(1 2 3 4 5 6 9) '(4 5 6))
                 '(9 3 2 1 4 5 6)))
        ((assert (union '() '(4 5 6))
                 '(4 5 6)))
        ((assert (union '(1 2 3 4) '())
                 '(4 3 2 1)))
        (else (display "All test passed")))
  (newline))
