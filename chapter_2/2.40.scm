#lang sicp

;; Exercise 2.40: Define a procedure unique-pairs that, given an integer n , generates the sequence of pairs ( i , j ) with 1 ≤ j < i ≤ n . Use unique-pairs to simplify the definition of prime-sum-pairs given above.

;; start: Prime
(define (square x) (* x x))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n)
         n)
        ((divides? test-divisor n)
         test-divisor)
        (else (find-divisor
               n
               (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))
;; end: Prime

(define (filter predicate list)
  (cond ((null? list) nil)
        ((predicate (car list))
         (cons (car list)
               (filter predicate (cdr list))))
        (else
         (filter predicate (cdr list)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op
                      initial
                      (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low
            (enumerate-interval
             (+ low 1)
             high))))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair)
        (cadr pair)
        (+ (car pair) (cadr pair))))

;; (define (prime-sum-pairs n)
;;   (map make-pair-sum
;;        (filter
;;         prime-sum?
;;         (flatmap
;;          (lambda (i)
;;            (map (lambda (j)
;;                   (list i j))
;;                 (enumerate-interval
;;                  1
;;                  (- i 1))))
;;          (enumerate-interval 1 n)))))


;; solution
(define (unique-pairs n)
  (flatmap
   (lambda (i)
     (map (lambda (j)
            (list i j))
          (enumerate-interval
           1
           (- i 1))))
   (enumerate-interval 1 n)))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter
        prime-sum?
        (unique-pairs n))))


;; test
(define (test)
  (cond ((equal? (prime-sum-pairs 5)
                 '((2 1 3) (3 2 5) (4 1 5) (4 3 7) (5 2 7)))
         (display "Test passed"))
        (else
         (display "Test failed!")))
  (newline))
