#lang sicp
;; Exercise 2.29: A binary mobile consists of two branches, a left
;; branch and a right branch. Each branch is a rod of a certain length,
;; from which hangs either a weight or another binary mobile. We can
;; represent a binary mobile using compound data by constructing it from
;; two branches (for example, using list):

;;     (define (make-mobile left right) (list left right))

;;     A branch is constructed from a length (which must be a number)
;; together with a structure, which may be either a number (representing
;; a simple weight) or another mobile:

;;     (define (make-branch length structure) (list length structure))


;; 4. Suppose we change the representation of mobiles so that the
;; constructors are

;; (define (make-mobile left right) (cons left right))

;; (define (make-branch length structure) (cons length structure))

;; How much do you need to change your programs to convert to the new
;; representation?


(define (make-mobile left right)
  (cons left right))

(define (make-branch length structure)
  (cons length structure))


;; 1. Write the corresponding selectors left-branch and right-branch,
;; which return the branches of a mobile, and branch-length and
;; branch-structure, which return the components of a branch.

(define (left-branch mobile) (car mobile))
(define (right-branch mobile) (cdr mobile))

(define (branch-length mobile) (car mobile))
(define (branch-structure mobile) (cdr mobile))


;; 2. Using your selectors, define a procedure total-weight that returns
;; the total weight of a mobile.
(define (branch-weight branch)
  (let ((struct (branch-structure branch)))
   (if (pair? struct)
       (total-weight struct)
       struct)))

(define (total-weight mobile)
  (+ (branch-weight (left-branch mobile))
     (branch-weight (right-branch mobile))))

(define (test-weight)
  (let ((m (make-mobile
            (make-branch
             10
             (make-mobile
              (make-branch 5 15)
              (make-branch 10 5)))
            (make-branch 10 20))))
    (if (= (total-weight m) 40)
        (display "Test case passed")
        (display "Test case failed")))
  (newline))


;; 3. A mobile is said to be balanced if the torque applied by its
;; top-left branch is equal to that applied by its top-right branch (that
;; is, if the length of the left rod multiplied by the weight hanging
;; from that rod is equal to the corresponding product for the right
;; side) and if each of the submobiles hanging off its branches is
;; balanced. Design a predicate that tests whether a binary mobile is
;; balanced.

(define (balanced? mobile)
  (cond ((not (pair? mobile)) #t)
        ((= (* (branch-length (left-branch mobile))
               (branch-weight (left-branch mobile)))
            (* (branch-length (right-branch mobile))
               (branch-weight (right-branch mobile))))
         (and (balanced? (branch-structure (left-branch mobile)))
              (balanced? (branch-structure (right-branch mobile)))))
        (else #f)))


(define (test-balanced)
  (let ((m (make-mobile
            (make-branch
             10
             (make-mobile
              (make-branch 5 15)
              (make-branch 10 5)))
            (make-branch 10 20)))
        (m2 (make-mobile
             (make-branch
              10
              (make-mobile
               (make-branch 5 10)
               (make-branch 10 5)))
             (make-branch 10 15))))
    (if (not (balanced? m))
        (display "First test case passed")
        (display "First test case failed"))
    (newline)

    (if (balanced? m2)
        (display "Second test case passed")
        (display "Second test case failed")))
  (newline))
