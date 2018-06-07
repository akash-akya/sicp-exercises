#lang sicp

(define (reverse list)
  (define (iter list acc)
    (if (null? list)
        acc
        (iter (cdr list)
              (cons (car list)
                    acc))))
  (iter list nil))


;; Test
(define (assert a b)
  (if (not (equal? a b))
      (display "failed!")
      #f))

(define (test)
  (cond ((assert (reverse (list 1 2 3 4 5))
                 (list 5 4 3 2 1)))
        ((assert (reverse (list 1))
                 (list 1)))
        ((assert (list)
                 (list)))
        (else (display "All tests pass")))
  (newline))
