#lang sicp

;; Exercise 2.39: Complete the following definitions of reverse (Exercise
;; 2.18) in terms of fold-right and fold-left from Exercise 2.38:

;;     (define (reverse sequence)
;;       (fold-right
;;        (lambda (x y) ⟨??⟩) nil sequence))

;;     (define (reverse sequence)
;;       (fold-left
;;        (lambda (x y) ⟨??⟩) nil sequence))


(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(define (fold-right op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (fold-right op
                      initial
                      (cdr sequence)))))

(define (reverse-right sequence)
  (fold-right
   (lambda (x y) (append y (list x))) nil sequence))

(define (reverse-left sequence)
  (fold-left
   (lambda (x y) (append (list y) x)) nil sequence))


;; test
(define (assert a b)
  (not (equal? a b)))

(define (test)
  (cond ((assert (reverse-left '(1 2 3))
                 '(3 2 1))
         (display "reverse-left failed"))
        ((assert (reverse-right '(1 2 3))
                 '(3 2 1))
         (display "reverse-right failed"))
        (else (display "All tests passed")))
  (newline))
