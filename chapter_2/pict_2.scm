#lang sicp
(#%require sicp-pict)

(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
          (bottom (beside (bl painter) (br painter))))
      (below bottom top))))

(define (flipped-pairs painter)
  (let ((combine4
         (square-of-four
          identity
          flip-vert
          identity
          flip-vert)))
    (combine4 painter)))




(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter
                               (- n 1))))
        (below painter
               (beside smaller smaller)))))

(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter
                                  (- n 1))))
        (beside painter
                (below smaller smaller)))))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter
                                (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right
                                   right))
              (corner (corner-split painter
                                    (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right
                         corner))))))


(define (sq-limit painter n)
  (let ((combine4
         (square-of-four
          flip-horiz
          identity
          (lambda (painter) (flip-horiz (flip-vert painter)))
          flip-vert)))
    (combine4 (corner-split painter n))))
