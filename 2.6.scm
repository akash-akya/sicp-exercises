#lang sicp

(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(define (to-num n)
  ((n inc) 0))

(define (print-number n)
  (display (to-num n))
  (newline))


;; test number rep

(define (err msg)
  (display "Failed ~ ")
  (display msg)
  (newline))

(define (assert a b)
  (if (not (= a b))
      (err a)
      #f))

(define (test-numbers)
  (cond ((assert (to-num zero) 0))
        ((assert (to-num (add-1 zero)) 1))
        ((assert (to-num (add-1 (add-1 zero))) 2))
        (else (display "All tests are passed")
              (newline))))


;; one & two!
(define one                    (lambda (f) (lambda (x) (f (((lambda (f) (lambda (x) x)) f) x))))         )
(define two (lambda (f) (lambda (x) (f (((lambda (f) (lambda (x) (f (((lambda (f) (lambda (x) x)) f) x)))) f) x)))))

(define (test-one-two)
  (cond ((assert (to-num one) 1))
        ((assert (to-num two) 2))
        (else (display "one & two tests passed")
              (newline))))


;; addition
(define (addition a b)
  (lambda (f)
    (lambda (x)
      (f (((a b) f) x)))))

(define (test-addition)
  (cond ((assert (to-num (addition one two)) 3))
        (else (display "Addition tests passed")
              (newline))))
