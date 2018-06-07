#lang sicp

(define (last-pair items)
  (if (null? (cdr items))
      items
      (last-pair (cdr items))))

(define (assert a b)
  (if (not (equal? a b))
      (display "failed!")
      #f))

(define (test)
  (cond ((assert (last-pair (list 1 2 3 4 5)) (list 5)))
        ((assert (last-pair (list 1)) (list 1)))
        (else (display "All tests pass")))
  (newline))
