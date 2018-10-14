#lang sicp
(#%require sicp-pict)
(#%require "utils.scm")

;; Exercise 4.42: Solve the following “Liars” puzzle (from Phillips 1934):

;; Five schoolgirls sat for an examination. Their parents—so they
;; thought—showed an undue degree of interest in the result. They
;; therefore agreed that, in writing home about the examination, each
;; girl should make one true statement and one untrue one. The
;; following are the relevant passages from their letters:

;;     Betty: “Kitty was second in the examination. I was only third.”
;;     Ethel: “You’ll be glad to hear that I was on top. Joan was second.”
;;     Joan: “I was third, and poor old Ethel was bottom.”
;;     Kitty: “I came out second. Mary was only fourth.”
;;     Mary: “I was fourth. Top place was taken by Betty.”

;; What in fact was the order in which the five girls were placed?

(define (require p)
  (if (not p) (amb)))

(define (distinct? items)
  (cond ((null? items) true)
        ((null? (cdr items)) true)
        ((member (car items) (cdr items)) false)
        (else (distinct? (cdr items)))))


(define (liars)
  (let ((betty (amb '(kitty . 2) '(betty . 3)))
        (ethel (amb '(ethel . 1) '(joa   . 2)))
        (joan  (amb '(joan  . 3) '(ethel . 5)))
        (kitty (amb '(kitty . 2) '(mary  . 4)))
        (mary  (amb '(mary  . 4) '(betty . 1))))
    (let ((ranking (list betty ethel joan kitty mary)))
      (require (distinct? (map cdr ranking)))
      (display ranking)
      (newline))))
