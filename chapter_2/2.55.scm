#lang sicp

;; Exercise 2.55: Eva Lu Ator types to the interpreter the expression
;; (car ''abracadabra)
;; To her surprise, the interpreter prints back quote. Explain.


;; (quote x) is a special form. It will skip the evaluation of the x.
;; Example: (quote (list abcd)) => (mcons 'list (mcons 'abcd '())).
;; It wont throw undefined abcd because its not evaluated yet.
;; 'x is short form of (quote x). (More like macro for quote)
;; Hence, ''abracadabra => (quote (quote abracadabra))
;; It wont evaluate inside (quote abracadabra). Hence its (list 'quote 'abracadabra)
;; (car (list 'quote 'abracadabra)) => 'quote
