#lang sicp

(define (for-each action list)
  (cond ((not (null? list))
         (action (car list))
         (for-each action (cdr list)))))


(for-each
 (lambda (x)
   (display x)
   (newline))
 (list 1 2 3 4))
