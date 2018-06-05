#lang sicp

(define (square x) (* x  x))

(define (non-trivial-sqrt a n)
  (and (not (= a 1))
       (not (= a (- n 1)))
       (= (remainder (square a) n) 1)))

;; (define (zero-if-non-trivial a n)
;;   (if (non-trivial-sqrt a n) 0 a))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((non-trivial-sqrt base m) 0)
        ((even? exp)
         (remainder
          (square (expmod base (/ exp 2) m))
          m))
        (else (remainder
               (* base (expmod base (- exp 1) m))
               m))))

(define (mr-test n)
  (define try-number (+ 1 (random (- n 2)))) ;; in the range 1..(n-1)
  (define exp (- n 1))
  (= (expmod try-number exp n) 1))


(define (mr-test-complete n)
  (define (mr a n)
    (= (expmod a (- n 1) n) 1))
  (define (test a n)
    (or (= a (- n 1))
        (and (mr a n)
             (test (+ a 1) n))))
  (test 1 n))


(define (check-in-range n)
  (define (disp a)
    (display a)
    (display "\n"))
  (define (try a)
    (cond ((> a n) nil)
          ((mr-test-complete a) (disp a)
           (try (+ 1 a)))
          (else (try (+ 1 a)))))
  (try 2))

;; (define (miller-rabin n)
;;   ())
