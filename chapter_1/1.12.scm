#lang sicp

(define (node-value row col)
  (cond ((or (= col row) (= col 1)) 1)
        (else (+ (node-value (- row 1) (- col 1))
                 (node-value (- row 1) col)))))

(define (print-row row col)
  (cond ((> col row) (display "\n"))
        (else (display " ")
              (display (node-value row col))
              (print-row row (+ col 1)))))

(define (pascal-tree rows current-row)
  (cond ((<= current-row rows)
         (print-row current-row 1)
         (pascal-tree rows (+ current-row 1)))))
