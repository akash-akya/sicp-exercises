#lang sicp
(#%require sicp-pict)
(#%require "utils.scm")

;; Exercise 4.40: In the multiple dwelling problem, how many sets of
;; assignments are there of people to floors, both before and after
;; the requirement that floor assignments be distinct? It is very
;; inefficient to generate all possible assignments of people to
;; floors and then leave it to backtracking to eliminate them. For
;; example, most of the restrictions depend on only one or two of the
;; person-floor variables, and can thus be imposed before floors have
;; been selected for all the people. Write and demonstrate a much more
;; efficient nondeterministic procedure that solves this problem based
;; upon generating only those possibilities that are not already ruled
;; out by previous restrictions. (Hint: This will require a nest of
;; let expressions.)


(define (require p)
  (if (not p) (amb)))

(define (distinct? items)
  (cond ((null? items) true)
        ((null? (cdr items)) true)
        ((member (car items) (cdr items)) false)
        (else (distinct? (cdr items)))))

(define (multiple-dwelling)
  (let ((baker (amb 1 2 3 4)))

    (let ((cooper (amb 2 3 4 5)))
      (require (distinct? (list baker cooper)))

      (let ((fletcher (amb 2 3 4)))
        (require (distinct? (list baker cooper fletcher)))
        (require (not (= (abs (- fletcher cooper)) 1)))

        (let ((smith (amb 1 2 3 4 5)))
          (require (distinct? (list baker cooper fletcher  smith)))
          (require (not (= (abs (- smith fletcher)) 1)))

          (let ((miller (amb 1 2 3 4 5)))
            (require (distinct? (list baker cooper fletcher smith miller)))
            (require (> miller cooper))

            (display (list (list 'baker baker)
                           (list 'cooper cooper)
                           (list 'fletcher fletcher)
                           (list 'miller miller)
                           (list 'smith smith)))))))))

(define (measure func)
  (let ((start (runtime)))
    (func)
    (- (runtime) start)))

(define (test-time)
  (let ((time (measure multiple-dwelling)))
    (newline)
    (display "Time: ")
    (display time))
  (newline))
