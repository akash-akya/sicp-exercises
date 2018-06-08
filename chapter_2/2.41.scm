#lang sicp

;; Exercise 2.41: Write a procedure to find all ordered triples of
;; distinct positive integers i , j , and k less than or equal to a given
;; integer n that sum to a given integer s .


;; solution
(define (contains list item)
  (if (null? list)
      #f
      (or (equal? (car list) item)
          (contains (cdr list) item))))

(define (pairs n)
  (flatmap
   (lambda (i)
     (map (lambda (j) (list i j))
          (filter (lambda (j) (not (= j i)))
                  (enumerate-interval 1 n))))
   (enumerate-interval 1 n)))

(define (triplets n)
  (flatmap
   (lambda (i)
     (map (lambda (j) (cons i j))
          (filter (lambda (j) (not (contains j i)))
                  (pairs n))))
   (enumerate-interval 1 n)))

(define (ordered-triplets n s)
  (filter
   (lambda (triplet)
     (= (accumulate + 0 triplet) s))
   (triplets n)))

;; test
(define (test)
  (cond ((equal?
          (ordered-triplets 5 10)
          '((1 4 5) (1 5 4) (2 3 5) (2 5 3) (3 2 5) (3 5 2) (4 1 5) (4 5 1) (5 1 4) (5 2 3) (5 3 2) (5 4 1)))
         (display "Test passed"))
        (else (display "Test failed")))
  (newline))
