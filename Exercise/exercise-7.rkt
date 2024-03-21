#lang racket
(provide bintree?-cps)
(module+ test (require rackunit))

#; CPS

; CSC324 2020 Fall - Exercise 7.
; Due Sunday November 22, 9PM

; This is an exercise in rewriting a given direct-style function to
; continuation-passing style.
;
; The function works on a representation of binary trees with data at internal
; nodes.  In this exercise, we use this representation (inductively defined):
;
; * A leaf node is represented by '(). (Arbitrary, convenient choice.)
;
; * An internal node is represented by (list datum left-child right-child),
;   where left-child and right-child are binary trees in this representation.
;   (No constraint on datum.)
;
; Below are an example and a counterexample.

; A picture of this is in exercise-7-pic.pdf
(define bintree-example-1
  (list 24
        (list 12
              (list 17 '() '())
              (list 22
                    '()
                    (list 29 '() '())))
        (list 15
              (list 10 '() '())
              '())))

(define illformed-example
  (list 24
        (list 12
              (list 17)  ; problem here is lacking children
              (list 22
                    '()
                    (list 29 '() '())))
        (list 15
              (list 10 '() '())
              '())))

; This is the direct-style function you will rewrite to CPS. It checks whether
; the input conforms to the representation.

(define (bintree? t)
  (match t
    ['() #t]
    [(list _ lt rt)
     (if (bintree? lt)
         (bintree? rt)
         #f)]
    [else #f]))

(module+ test
  (check-true (bintree? bintree-example-1))
  (check-false (bintree? illformed-example)))

; Code up the CPS version below.

(define (bintree?-cps t cont)
  (match t
    ['() (cont #t)]
    [(list _ lt rt)
     (cont (if (bintree?-cps lt identity)
               (bintree?-cps rt identity)
               #f))]
    [else (cont #f)]))

; Uncomment when you're ready to test:
(module+ test
  (check-true (bintree?-cps bintree-example-1 identity))
  (check-false (bintree?-cps bintree-example-1 not))
  (check-false (bintree?-cps illformed-example identity)))
