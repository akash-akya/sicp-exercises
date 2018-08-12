#lang sicp
(#%require "utils.scm")

;; Exercise 3.23: A deque (“double-ended queue”) is a sequence in
;; which items can be inserted and deleted at either the front or the
;; rear. Operations on deques are the constructor make-deque, the
;; predicate empty-deque?, selectors front-deque and rear-deque, and
;; mutators front-insert-deque!, rear-insert-deque!,
;; front-delete-deque!, rear-delete-deque!. Show how to represent
;; deques using pairs, and give implementations of the operations.151
;; All operations should be accomplished in Θ ( 1 ) steps.

;; doubly linked lists
(define (make-entry data left right)
  (list data left right))

(define (set-left! entry left)
  (set-car! (cdr entry) left))

(define (set-right! entry right)
  (set-car! (cddr entry) right))

(define (data entry) (car entry))

(define (left entry) (cadr entry))

(define (right entry) (caddr entry))

(define (make-deque)
  (let ((front-ptr nil)
        (rear-ptr nil))

    (define (set-front-ptr! item)
      (set! front-ptr item))

    (define (set-rear-ptr! item)
      (set! rear-ptr item))

    (define (empty-queue?)
      (null? front-ptr))

    (define (front-insert-deque! element)
      (let ((entry (make-entry element nil nil)))
        (cond ((empty-queue?)
               (set-front-ptr! entry)
               (set-rear-ptr! entry))
              (else
               (set-right! entry front-ptr)
               (set-left! front-ptr entry)
               (set-front-ptr! entry)))))

    (define (rear-insert-deque! element)
      (let ((entry (make-entry element nil nil)))
        (cond ((empty-queue?)
               (set-front-ptr! entry)
               (set-rear-ptr! entry))
              (else
               (set-right! rear-ptr entry)
               (set-left! entry rear-ptr)
               (set-rear-ptr! entry)))))

    (define (front-delete-deque!)
      (cond ((empty-queue?)
             (error "DELETE! called with empty deque"))
            (else
             (set-left! (right front-ptr) nil)
             (set-front-ptr! (right front-ptr)))))

    (define (rear-delete-deque!)
      (cond ((empty-queue?)
             (error "DELETE! called with empty deque"))
            (else
             (set-right! (left rear-ptr) nil)
             (set-rear-ptr! (left rear-ptr)))))

    (define (front-deque)
      (if (empty-queue?)
          nil
          (data front-ptr)))

    (define (rear-deque)
      (if (empty-queue?)
          nil
          (data rear-ptr)))

    (define (print-deque)
      (define (iter entry)
        (cond ((null? entry)
               (newline))
              (else
               (display (data entry))
               (display " ")
               (iter (right entry)))))
      (newline)
      (iter front-ptr))

    (define (dispatch m)
      (cond
       ((eq? 'insert-front m) front-insert-deque!)
       ((eq? 'insert-rear m) rear-insert-deque!)
       ((eq? 'delete-front m) front-delete-deque!)
       ((eq? 'delete-rear m) rear-delete-deque!)
       ((eq? 'rear-deque m) rear-deque)
       ((eq? 'front-deque m) front-deque)
       ((eq? 'print-deque m) print-deque)
       (else
        (error "Invalid option!" m))))

    dispatch))

(define (insert-front q elem) ((q 'insert-front) elem))
(define (insert-rear q elem) ((q 'insert-rear) elem))
(define (delete-front q) ((q 'delete-front)))
(define (delete-rear q) ((q 'delete-rear)))
(define (rear-deque q) ((q 'rear-deque)))
(define (front-deque q) ((q 'front-deque)))
(define (print-deque q) ((q 'print-deque)))

;; test
(define (run-test)
  (define q1 (make-deque))
  (insert-front q1 'a)
  (insert-front q1 'b)
  (insert-front q1 'c)
  (insert-rear q1 'd)

  (display "Deque: ")
  (print-deque q1)

  (test-equal (front-deque q1) 'c)
  (test-equal (rear-deque q1) 'd)

  (delete-front q1)
  (test-equal (front-deque q1) 'b)

  (delete-rear q1)
  (test-equal (rear-deque q1) 'a)

  (insert-front q1 'e)
  (insert-rear q1 'f)

  (print-deque q1))
