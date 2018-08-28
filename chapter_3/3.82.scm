#lang sicp
(#%require "utils.scm")
(#%require "stream-utils.scm")

;; Exercise 3.82: Redo Exercise 3.5 on Monte Carlo integration in
;; terms of streams. The stream version of estimate-integral will not
;; have an argument telling how many trials to perform. Instead, it
;; will produce a stream of estimates based on successively more
;; trials.

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))


(define (monte-carlo experiment-stream
                     passed
                     failed)
  (define (next passed failed)
    (cons-stream
     (/ passed (+ passed failed))
     (monte-carlo
      (stream-cdr experiment-stream)
      passed
      failed)))
  (if (stream-car experiment-stream)
      (next (+ passed 1) failed)
      (next passed (+ failed 1))))

(define (create-experiment-stream P x1 x2 y1 y2)
  (cons-stream
   (P (random-in-range x1 x2) (random-in-range y1 y2))
   (create-experiment-stream P x1 x2 y1 y2)))

(define (area x1 x2 y1 y2)
  (* (- y2 y1) (- x2 x1)))

(define (estimate-integral P x1 x2 y1 y2)
  (stream-map
   (lambda (success-rate)
     (* (area x1 x2 y1 y2) success-rate))
   (monte-carlo (create-experiment-stream P x1 x2 y1 y2) 0 0)))

(define (create-predicate cx cy radius)
  (lambda (x y)
    (not (> (+ (square (- x cx))
               (square (- y cy)))
            (square radius)))))

(define (estimate-pi)
                                       ;; cx   cy   radius  x1  x2   y1  y2
  (/ (stream-ref (estimate-integral (create-predicate 1000 1000 1000)   0   2000 0   2000) 100000)
     (square 1000.0)))

;; test
(define (test)
  (< (abs (- (estimate-pi) 3.14285))
     0.01))




;; test
