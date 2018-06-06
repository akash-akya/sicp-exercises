#lang sicp

;; Exercise 1.45: We saw in 1.3.3 that attempting to compute square roots by naively
;; finding a fixed point of y ↦ x / y does not converge, and that this can be fixed by
;; average damping. The same method works for finding cube roots as fixed points of the
;; average-damped y ↦ x / y 2 . Unfortunately, the process does not work for fourth
;; roots—a single average damp is not enough to make a fixed-point search for y ↦ x / y
;; 3 converge. On the other hand, if we average damp twice (i.e., use the average damp
;; of the average damp of y ↦ x / y 3 ) the fixed-point search does converge. Do some
;; experiments to determine how many average damps are required to compute n th roots as
;; a fixed-point search based upon repeated average damping of y ↦ x / y n − 1 . Use
;; this to implement a simple procedure for computing n th roots using fixed-point,
;; average-damp, and the repeated procedure of Exercise 1.43. Assume that any arithmetic
;; operations you need are available as primitives.

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define a 0)
  (define (closs-enough? x y)
    (set! a (inc a))
    (< (abs (- x y)) tolerance))

  (define (try guess)
    (let ((new-guess (f guess)))
      (if (closs-enough? guess new-guess)
          new-guess
          (try (f new-guess)))))

  (try first-guess))

(define (square x) (* x x))

(define (compose f g)
  (lambda (x)
    (f (g x))))

(define (repeated f n)
  (if (= n 0)
      identity  ; apply the f 'zero' times. ie return the argument as it is
      (compose f
               (repeated f (- n 1)))))


(define (average-damp f)
  (lambda (x)
    (/ (+ x (f x))
       2)))

(define (root-eqation n x)
  (lambda (y)
    (/ x (expt y (dec n)))))

(define (nth-root n x)
  (fixed-point
   ((repeated
     average-damp
     (floor (/ (log n)
               (log 2))))
    (root-eqation n x))
    1.0))


;;;;; Find how many average damping required to find the nth-root
(define (modified-fixed-point f first-guess)
  (define a 0)
  (define (closs-enough? x y)
    (set! a (inc a))
    (or (< (abs (- x y)) tolerance)
        (> a 1500)))

  (define (try guess)
    (let ((new-guess (f guess)))
      (if (closs-enough? guess new-guess)
          new-guess
          (try (f new-guess)))))

  (try first-guess))

(define (random-init-guess)
  (* (inc (random 5000)) 1.0))

(define (find-root num nth-root repeat)
    (modified-fixed-point
     ((repeated average-damp repeat)
      (root-eqation nth-root num))
     (random-init-guess)))

(define (try-dampings num nth-root damp-count result)
  (if (= (round (find-root num nth-root damp-count)) result)
      damp-count
      (try-dampings num nth-root (inc damp-count) result)))

(define (test-number n nth-root)
  (try-dampings (expt n nth-root) nth-root 0 n))

(define (avg-damping-count nth-root)
  (define (maximize-avg-damping-count i cur-max)
    (if (= i 100)
        cur-max
        (maximize-avg-damping-count
         (inc i)
         (max (test-number (inc (random 1000)) nth-root)
              cur-max))))
  (maximize-avg-damping-count 0 0))

(define (show i count)
  (display i)
  (display " - ")
  (display count)
  (newline))

(define (find-avg-damping-for-range n)
  (define (test-numer i last)
    (let ((count (avg-damping-count i)))
      (cond ((> count last)
             (show i count)
             count)
            (else last))))

  (define (iter i last)
    (if (< i n)
        (iter (inc i) (test-numer i last))))

  (iter 2 0))
