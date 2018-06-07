#lang sicp

(define (prime-factor-count n prime)
  (define (iter i num)
    (if (> (remainder num prime) 0)
        i
        (iter (inc i) (/ num prime))))
  (iter 0 n))

(define (cons a b)
  (* (expt 2 a)
     (expt 3 b)))

(define (car c)
  (prime-factor-count c 2))

(define (cdr c)
  (prime-factor-count c 3))

;; Test
(define (test)
  (if (and
       (= (car (cons 3 4))
          3)
       (= (cdr (cons 3 4))
          4))
      (display "All tests are passed")
      (display "Tests failed!"))
  (newline))
