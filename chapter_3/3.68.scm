#lang sicp
(#%require "utils.scm")

;;  Exercise 3.68: Louis Reasoner thinks that building a stream of
;;  pairs from three parts is unnecessarily complicated. Instead of
;;  separating the pair ( S 0 , T 0 ) from the rest of the pairs in
;;  the first row, he proposes to work with the whole first row, as
;;  follows:

;; (define (pairs s t)
;;   (interleave
;;    (stream-map
;;     (lambda (x)
;;       (list (stream-car s) x))
;;     t)
;;    (pairs (stream-cdr s)
;;           (stream-cdr t))))

;; Does this work? Consider what happens if we evaluate (pairs
;; integers integers) using Louis’s definition of pairs.



;; Answer:

;; stream-cons _does not_ evaluate second argument. Second argument
;; execution will be delayed as cons-stream is a special form. In the
;; new version however interleave try to evaluate second argument as
;; it is not a special form (arguments must be evaluated before
;; passing), which happens to be recursive, interleave needs (pairs
;; (cdr ..) (cdr ..)) call, which intern have interleave with another
;; (pairs (cdr (cdr ...)) (cdr (cdr ...))) call.
