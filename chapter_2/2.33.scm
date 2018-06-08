#lang sicp

;; Exercise 2.33: Fill in the missing expressions to complete the
;; following definitions of some basic list-manipulation operations as
;; accumulations:

;; (define (map p sequence)
;;   (accumulate (lambda (x y) ⟨??⟩)
;;               nil sequence))

;; (define (append seq1 seq2)
;;   (accumulate cons ⟨??⟩ ⟨??⟩))

;; (define (length sequence)
;;   (accumulate ⟨??⟩ 0 sequence))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op
                      initial
                      (cdr sequence)))))


(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y))
              nil sequence))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length sequence)
  (accumulate (lambda (_ s) (inc s)) 0 sequence))


;; test
(define (err msg)
  (display msg)
  (newline))

(define (test)
  (cond ((not (equal? (map inc '(1 2 3)) '( 2 3 4)))
         (err "map failed"))
        ((not (equal? (append '(1 2) '(3 4)) '(1 2 3 4)))
         (err "append failed"))
        ((not (equal? (length '(1 2 3)) 3))
         (err "length failed"))
        (else (display "All tests passed")))
  (newline))
