#lang sicp
(#%require "utils.scm")

(define (stream-car stream)
  (car stream))

(define (stream-cdr stream)
  (force (cdr stream)))

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc
                    (map stream-cdr
                         argstreams))))))

(define (display-stream stream)
  (if (stream-null? stream)
      (newline)
      (begin
        (display (stream-car stream))
        (display " ")
        (display-stream (stream-cdr stream)))))

(define (stream-to-list stream)
  (if (stream-null? stream)
      nil
      (cons (stream-car stream) (stream-to-list (stream-cdr stream)))))

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))


(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define (take-n stream n)
  (if (<= n 0)
      nil
      (cons (stream-car stream)
            (take-n (stream-cdr stream)
                    (- n 1)))))

(define (display-stream-n stream n)
  (if (= n 0)
      (newline)
      (begin
        (display (stream-car stream)) (display "  ")
        (display-stream-n (stream-cdr stream) (- n 1)))))

(define (stream-filter pred stream)
  (cond ((stream-null? stream)
         the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream
          (stream-car stream)
          (stream-filter
           pred
           (stream-cdr stream))))
        (else (stream-filter
               pred
               (stream-cdr stream)))))

(define ones (cons-stream 1 ones))

(define integers
  (cons-stream 1 (add-streams ones integers)))

(#%provide stream-car)
(#%provide stream-cdr)
(#%provide stream-map)
(#%provide display-stream)
(#%provide stream-to-list)
(#%provide stream-ref)
(#%provide add-streams)
(#%provide display-stream-n)
(#%provide stream-filter)
(#%provide take-n)
(#%provide integers)
