#lang sicp

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


(define (nth-root n x)
  (define (root-eqation n x)
    (lambda (y)
      (/ x (expt y (dec n)))))

  (fixed-point
   ((repeated average-damp
              (floor (/ (log n)
                        (log 2))))
    (root-eqation n x))
    1.0))

;;;;; tests to get the min repeatation count

(define (get-damping-count nth-root)
  (define (root-eqation x)
    (lambda (y)
      (/ x (expt y (- nth-root 1)))))

  (define (random-init-guess)
    (* (inc (random 5000)) 1.0))

  (define (find-root num repeat)
    (fixed-point
     ((repeated average-damp repeat)
      (root-eqation num))
     (random-init-guess)))

  (define (try-dampings num damp-count result)
    (if (= (round (find-root num damp-count))
           result)
        damp-count
        (try-dampings num
                      (inc damp-count)
                      result)))

  (define (test-number n)
    (try-dampings (expt n nth-root)
                  0
                  n))

  (define (try-i-numbers i cur-max)
    (if (= i 50)
        cur-max
        (try-i-numbers (inc i)
                       (max (test-number (+ 1 (random 5000)))
                            cur-max))))

  (try-i-numbers 0 0))


(define (try-n-roots)
  (define (show i count)
    (display i)
    (display " - ")
    (display count)
    (newline)
    #t)

  (define last 0)
  (define (test i)
    (let ((count (get-damping-count i)))
     (if (> count last)
        (and (show i count)
             (set! last count))))
    #t)

  (define (iter i)
    (if (= i 34)
        nil
        (and (test i)
             (iter (+ i 1)))))
  (iter 2))
