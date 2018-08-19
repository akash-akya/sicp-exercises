#lang sicp
(#%require "utils.scm")
(#%require "stream-utils.scm")

;;  Exercise 3.70: It would be nice to be able to generate streams in
;;  which the pairs appear in some useful order, rather than in the
;;  order that results from an ad hoc interleaving process. We can use
;;  a technique similar to the merge procedure of Exercise 3.56, if we
;;  define a way to say that one pair of integers is “less than”
;;  another. One way to do this is to define a “weighting function” W
;;  ( i , j ) and stipulate that ( i 1 , j 1 ) is less than ( i 2 , j
;;  2 ) if W ( i 1 , j 1 ) < W ( i 2 , j 2 ) . Write a procedure
;;  merge-weighted that is like merge, except that merge-weighted
;;  takes an additional argument weight, which is a procedure that
;;  computes the weight of a pair, and is used to determine the order
;;  in which elements should appear in the resulting merged stream.197
;;  Using this, generalize pairs to a procedure weighted-pairs that
;;  takes two streams, together with a procedure that computes a
;;  weighting function, and generates the stream of pairs, ordered
;;  according to weight. Use your procedure to generate

;; the stream of all pairs of positive integers ( i , j ) with i ≤ j
;; ordered according to the sum i + j , the stream of all pairs of
;; positive integers ( i , j ) with i ≤ j , where neither i nor j is
;; divisible by 2, 3, or 5, and the pairs are ordered according to the
;; sum 2 i + 3 j + 5 i j .


(define (merge-weighted s1 s2 weight)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (cond ((< (weight s1car) (weight s2car))
                  (cons-stream
                   s1car
                   (merge-weighted (stream-cdr s1) s2 weight)))
                 (else
                  (cons-stream
                   s2car
                   (merge-weighted s1 (stream-cdr s2) weight))))))))


(define ones (cons-stream 1 ones))

(define integers
  (cons-stream 1 (add-streams ones integers)))

(define (weighted-pairs s t weight)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (merge-weighted
    (stream-map (lambda (x)
                  (list (stream-car s) x))
                (stream-cdr t))
    (weighted-pairs (stream-cdr s) (stream-cdr t) weight)
    weight)))

;; 1
(define (weight-func p) (+ (car p) (cadr p)))

(define ordered-pairs
  (weighted-pairs integers integers weight-func))

;; 2
(define (weight-func-2 p)
  (let ((i (car p))
        (j (cadr p)))
    (+ (* 2 i)
       (* 3 j)
       (* 5 i j))))

(define (generate-inegers)
  (stream-filter
   (lambda (x)
     (not (or (= (remainder x 2) 0)
              (= (remainder x 3) 0)
              (= (remainder x 5) 0))))
   integers))


(define ordered-pairs-2
  (let ((s (generate-inegers)))
   (weighted-pairs s s weight-func-2)))

;; test
(define (test)
  (define (iter w last-weight)
    (if (null? w)
        #t
        (and (>= (car w) last-weight)
             (iter (cdr w) (car w)))))

  (let  ((w (map weight-func (take-n ordered-pairs 100)))
         (w2 (map weight-func-2 (take-n ordered-pairs-2 100))))
    (test-equal (iter w 0)  #t)
    (test-equal (iter w2 0)  #t)))
