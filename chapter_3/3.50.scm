#lang sicp
(#%require "utils.scm")

;; Exercise 3.50: Complete the following definition, which generalizes
;; stream-map to allow procedures that take multiple arguments,
;; analogous to map in 2.2.1, Footnote 78.

;; (define (stream-map proc . argstreams)
;;   (if (⟨??⟩ (car argstreams))
;;       the-empty-stream
;;       (⟨??⟩
;;        (apply proc (map ⟨??⟩ argstreams))
;;        (apply stream-map
;;               (cons proc
;;                     (map ⟨??⟩
;;                          argstreams))))))

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

;; test
(define (stream-enumerate-interval start end)
  (if (>= start end)
      the-empty-stream
      (cons-stream
       start
       (stream-enumerate-interval (+ start 1) end))))

(define (test)
  (let ((stream-a (stream-enumerate-interval 0 10))
        (stream-b (stream-enumerate-interval 10 20))
        (stream-c (stream-enumerate-interval 20 30)))
    (test-equal (stream-to-list (stream-map + stream-a stream-b stream-c))
      '(30 33 36 39 42 45 48 51 54 57))))
