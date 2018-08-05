#lang sicp
(#%require "utils.scm")

;; Exercise 3.22: Instead of representing a queue as a pair of
;; pointers, we can build a queue as a procedure with local state. The
;; local state will consist of pointers to the beginning and the end
;; of an ordinary list. Thus, the make-queue procedure will have the
;; form

;; (define (make-queue)
;;   (let ((front-ptr … )
;;         (rear-ptr … ))
;;     ⟨definitions of internal procedures⟩
;;     (define (dispatch m) …)
;;     dispatch))

;; Complete the definition of make-queue and provide implementations
;; of the queue operations using this representation.


(define (make-queue)
  (let ((front-ptr nil)
        (rear-ptr nil))

    (define (set-front-ptr! item)
      (set! front-ptr item))

    (define (set-rear-ptr! item)
      (set! rear-ptr item))

    (define (empty-queue?)
      (null? front-ptr))

    (define (insert-queue! item)
      (let ((new-pair (cons item '())))
        (cond ((empty-queue?)
               (set-front-ptr! new-pair)
               (set-rear-ptr! new-pair))
              (else (set-cdr! rear-ptr new-pair)
                (set-rear-ptr! new-pair)))))

    (define (delete-queue!)
      (cond ((empty-queue?)
             (error "DELETE! called with
                 an empty queue"))
            (else
             (set-front-ptr! (cdr front-ptr)))))

    (define (print-queue)
      (define (iter list)
        (cond ((null? list)
               (newline))
              (else
               (display (car list))
               (display " ")
               (iter (cdr list)))))
      (newline)
      (iter front-ptr))


    (define (dispatch m)
      (cond ((eq? 'insert m) insert-queue!)
            ((eq? 'delete m) delete-queue!)
            ((eq? 'print m) print-queue)
            (else
             (error "Invalid option!" m))))
    dispatch))

(define (delete-queue! q)
  ((q 'delete)))

(define (insert-queue! q item)
  ((q 'insert) item))

(define (print-queue q)
  ((q 'print)))


;; test
(define (test)
  (define q1 (make-queue))
  (insert-queue! q1 'a)
  (insert-queue! q1 'b)
  (insert-queue! q1 'c)

  (print-queue q1)

  (delete-queue! q1)
  (print-queue q1)

  (delete-queue! q1)
  (print-queue q1))

(test)
