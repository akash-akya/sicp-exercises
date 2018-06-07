#lang sicp

;; Exercise 2.27: Modify your reverse procedure of Exercise 2.18 to
;; produce a deep-reverse procedure that takes a list as argument and
;; returns as its value the list with its elements reversed and with all
;; sublists deep-reversed as well. For example,

;; (define x
;;   (list (list 1 2) (list 3 4)))

;; x
;; ((1 2) (3 4))

;; (reverse x)
;; ((3 4) (1 2))

;; (deep-reverse x)
;; ((4 3) (2 1))


(define (deep-reverse x)
  (cond ((not (pair? x)) x)
        ((null? (cdr x))
         (deep-reverse (car x)))
        (else
         (list (deep-reverse (cdr x))
               (deep-reverse (car x))))))


(define (test)
  (if (equal? (deep-reverse (list (list 1 2) (list 3 4)))
              (list (list 4 3) (list 2 1)))
      (display "Working!")
      (display "failed"))
  (newline))
