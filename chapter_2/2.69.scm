#lang sicp
;; Exercise 2.69: The following procedure takes as its argument a list
;; of symbol-frequency pairs (where no symbol appears in more than one
;; pair) and generates a Huffman encoding tree according to the
;; Huffman algorithm.

;; (define (generate-huffman-tree pairs)
;;   (successive-merge
;;    (make-leaf-set pairs)))

;; Make-leaf-set is the procedure given above that transforms the list of
;; pairs into an ordered set of leaves. Successive-merge is the procedure
;; you must write, using make-code-tree to successively merge the
;; smallest-weight elements of the set until there is only one element
;; left, which is the desired Huffman tree. (This procedure is slightly
;; tricky, but not really complicated. If you find yourself designing a
;; complex procedure, then you are almost certainly doing something
;; wrong. You can take significant advantage of the fact that we are
;; using an ordered set representation.)

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf x) (cadr x))

(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left)
                (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))

(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set)))
         (cons x set))
        (else
         (cons (car set)
               (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set
         (make-leaf (car pair)    ; symbol
                    (cadr pair))  ; frequency
         (make-leaf-set (cdr pairs))))))


;; Solution
(define (update-list list branch)
  (adjoin-set branch (cddr list)))

(define (successive-merge elms)
  (cond ((null? (cdr elms))
         (car elms))
        (else
         (successive-merge
          (update-list
           elms
           (make-code-tree (car elms) (cadr elms)))))))

(define (generate-huffman-tree pairs)
  (successive-merge
   (make-leaf-set pairs)))

(define sample-pairs '((A 4) (B 2) (C 1) (D 1)))

(define sample-tree
  (make-code-tree
   (make-leaf 'A 4)
   (make-code-tree
    (make-leaf 'B 2)
    (make-code-tree
     (make-leaf 'D 1)
     (make-leaf 'C 1)))))

;; test
;; (display (generate-huffman-tree sample-pairs))

;; (define (test)
;;   (test-equal (generate-huffman-tree sample-pairs)
;;     sample-tree))

;; provide for next exercises
(define (contains symbol symbols)
  (cond ((null? symbols) #f)
        ((equal? symbol (car symbols)) #t)
        (else (contains symbol (cdr symbols)))))

(define (encode-symbol symbol tree)
  (cond ((leaf? tree) nil)
        ((contains symbol (symbols (left-branch tree)))
         (cons 0 (encode-symbol symbol (left-branch tree))))
        ((contains symbol (symbols (right-branch tree)))
         (cons 1 (encode-symbol symbol (right-branch tree))))
        (else (error "Invalid symbol" symbol))))

(define (encode message tree)
  (if (null? message)
      '()
      (append
       (encode-symbol (car message)
                      tree)
       (encode (cdr message) tree))))

(#%provide make-leaf)
(#%provide leaf?)
(#%provide symbol-leaf)
(#%provide weight-leaf)
(#%provide make-code-tree)
(#%provide left-branch)
(#%provide right-branch)
(#%provide symbols)
(#%provide weight)
(#%provide adjoin-set)
(#%provide make-leaf-set)
(#%provide update-list)
(#%provide successive-merge)
(#%provide generate-huffman-tree)
(#%provide encode)
