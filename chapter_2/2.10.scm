#lang sicp

(define (same-parity . numbers)
  (define predicate
    (if (even? (car numbers))
        even?
        odd?))
  (define (filter numbers)
    (cond ((null? numbers) nil)
          ((predicate (car numbers))
           (cons (car numbers)
                 (filter (cdr numbers))))
          (else
           (filter (cdr numbers)))))
  (filter numbers))
