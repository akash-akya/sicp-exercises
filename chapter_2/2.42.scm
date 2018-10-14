#lang sicp
(#%require "utils.scm")
;; Exercise 2.42: The “eight-queens puzzle” asks how to place eight
;; queens on a chessboard so that no queen is in check from any
;; other (i.e., no two queens are in the same row, column, or
;; diagonal). One possible solution is shown in Figure 2.8. One way to
;; solve the puzzle is to work across the board, placing a queen in each
;; column. Once we have placed k − 1 queens, we must place the k th queen
;; in a position where it does not check any of the queens already on the
;; board. We can formulate this approach recursively: Assume that we have
;; already generated the sequence of all possible ways to place k − 1
;; queens in the first k − 1 columns of the board. For each of these
;; ways, generate an extended set of positions by placing a queen in each
;; row of the k th column. Now filter these, keeping only the positions
;; for which the queen in the k th column is safe with respect to the
;; other queens. This produces the sequence of all ways to place k queens
;; in the first k columns. By continuing this process, we will produce
;; not only one solution, but all solutions to the puzzle.  SVG

;;     Figure 2.8: A solution to the eight-queens puzzle.

;; We implement this solution as a procedure queens, which returns a
;; sequence of all solutions to the problem of placing n queens on an n ×
;; n chessboard. Queens has an internal procedure queen-cols that returns
;; the sequence of all ways to place queens in the first k columns of the
;; board.

;; (define (queens board-size)
;;   (define (queen-cols k)
;;     (if (= k 0)
;;         (list empty-board)
;;         (filter
;;          (lambda (positions)
;;            (safe? k positions))
;;          (flatmap
;;           (lambda (rest-of-queens)
;;             (map (lambda (new-row)
;;                    (adjoin-position
;;                     new-row
;;                     k
;;                     rest-of-queens))
;;                  (enumerate-interval
;;                   1
;;                   board-size)))
;;           (queen-cols (- k 1))))))
;;   (queen-cols board-size))

;; In this procedure rest-of-queens is a way to place k − 1 queens in the
;; first k − 1 columns, and new-row is a proposed row in which to place
;; the queen for the k th column. Complete the program by implementing
;; the representation for sets of board positions, including the
;; procedure adjoin-position, which adjoins a new row-column position to
;; a set of positions, and empty-board, which represents an empty set of
;; positions. You must also write the procedure safe?, which determines
;; for a set of positions, whether the queen in the k th column is safe
;; with respect to the others. (Note that we need only check whether the
;; new queen is safe—the other queens are already guaranteed safe with
;; respect to each other.)


(define empty-board '(0 0 0 0 0 0 0 0))

(define (col board k)
  (if (= k 0)
      (car board)
      (col (cdr board) (- k 1))))

(define (row col k) (col col k))

(define (get board c r)
  (row (col board c) r))

(define (set-nth l pos val)
  (define (iter p l)
    (cond ((null? l) nil)
          ((= p pos) (cons val (cdr l)))
          (else (cons (car l)
                      (iter (+ p 1) (cdr l))))))
  (iter 1 l))

(define (adjoin-position row k rest)
  (set-nth rest k row))

(define (assign-pos l)
  (define (iter i l)
    (if (null? l)
        nil
        (cons (cons i (car l))
              (iter (+ i 1)
                    (cdr l)))))
  (iter 1 l))

(define (valid-diagonal col row rest)
  (not
   (null?
    (filter
     (lambda (p)
       (= (abs (- (car p) col))
          (abs (- (cdr p) row))))
     (assign-pos rest)))))

(define (safe? k positions)
  (let ((row (car (reverse positions)))
        (rest (cdr (reverse positions))))
    (cond ((member row rest) #f)
          ((valid-diagonal k row rest)))))

(define (accumulate op initial l)
  (if (null? l)
      initial
      (op (car l)
          (accumulate op
                      initial
                      (cdr l)))))

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low
            (enumerate-interval
             (+ low 1)
             high))))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))


(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions)
           (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position
                    new-row
                    k
                    rest-of-queens))
                 (enumerate-interval
                  1
                  board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))



;; test
;; (define (test)
;;   (cond (( )
;;          (display "Test passed"))
;;         (else (display "Test failed")))
;;   (newline))
