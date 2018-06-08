#lang sicp
;; Exercise 2.37: Suppose we represent vectors v = ( v i ) as
;; sequences of numbers, and matrices m = ( m i j ) as sequences of
;; vectors (the rows of the matrix). For example, the matrix
;;     ( 1 2 3 4 4 5 6 6 6 7 8 9 ) is represented as the sequence ((1 2 3
;; 4) (4 5 6 6) (6 7 8 9)). With this representation, we can use sequence
;; operations to concisely express the basic matrix and vector
;; operations. These operations (which are described in any book on
;; matrix algebra) are the following:
;; (dot-product v w) returns the sum Σ i v i w i ;
;; (matrix-*-vector m v) returns the vector t , where t i = Σ j m i j v j ;
;; (matrix-*-matrix m n) returns the matrix p , where p i j = Σ k m i k n k j ;
;; (transpose m) returns the matrix n , where n i j = m j i .

;; We can define the dot product as

;;     (define (dot-product v w)
;;       (accumulate + 0 (map * v w)))

;; Fill in the missing expressions in the following procedures for
;; computing the other matrix operations. (The procedure accumulate-n is
;; defined in Exercise 2.36.)

;;     (define (matrix-*-vector m v)
;;       (map ⟨??⟩ m))

;;     (define (transpose mat)
;;       (accumulate-n ⟨??⟩ ⟨??⟩ mat))

;;     (define (matrix-*-matrix m n)
;;       (let ((cols (transpose n)))
;;         (map ⟨??⟩ m)))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op
                      initial
                      (cdr sequence)))))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

(define (matrix-*-vector m v)
  (map (lambda (y)
         (accumulate +
                     0
                     (accumulate-n * 1 (list v y))))  m))

(define (transpose mat)
  (accumulate-n (lambda (a b)
                  (cons a b)) nil mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map
     (lambda (a) (matrix-*-vector cols a))
     m)))


;; test
(define (err msg)
  (display msg))

(define (test)
  (cond ((not (equal? (matrix-*-vector '((1 2) (2 4)) '(10 20))
                 '(50 100)))
         (err "matrix-*-vector failed"))
        ((not (equal? (transpose '((1 2) (3 4)))
                      '((1 3) (2 4))))
         (err "transpose failed"))
        ((not (equal? (matrix-*-matrix '((1 1) (3 4)) '((1 2) (3 4)))
                      '((4 6) (15 22))))
         (err "matrix-*-matrix failed"))
        (else (display "All tests passed")))
  (newline))
