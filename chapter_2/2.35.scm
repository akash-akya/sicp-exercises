#lang sicp

;; Exercise 2.35: Redefine count-leaves from 2.2.2 as an accumulation:

;;     (define (count-leaves t)
;;       (accumulate ⟨??⟩ ⟨??⟩ (map ⟨??⟩ ⟨??⟩)))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op
                      initial
                      (cdr sequence)))))

(define (count-leaves tree)
  (accumulate + 0 (map (lambda (subtree)
                         (if (pair? subtree)
                             (count-leaves subtree)
                             1))
                       tree)))


;; test
(define (test)
  (define x (cons (list 1 2) (list 3 4)))
  (cond ((= (count-leaves (list x x))
            8)
         (display "Test passed"))
        (else (display "Test failed")))
  (newline))
