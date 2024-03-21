#lang racket

; More exercises on rewriting functions to CPS.

; Some setup.

(define (plus-cps a b cont)
  (cont (+ a b)))

(define (minus-cps a b cont)
  (cont (- a b)))

(define (times-cps a b cont)
  (cont (* a b)))

(define (sin-cps x cont)
  (cont (sin x)))


; Q1: A deep expression tree, but otherwise ordinary, and no recursion.

(define (mypoly x)
  (+ (+ (* x x)
        (* 2 x))
     1))

; Note that when you rewrite to CPS, you should respect the original evaluation order,
; e.g., mypoly uses this order:
; 1. compute x*x
; 2. compute 2*x
; 3. add them
; 4. add 1
; So the CPS version should aim for that order, e.g., try not to do 2*x first.

; Answer:

(define (mypoly-cps x cont)
  "unimplemented")

; Q2: A recursive function, and at least one recursive call is non-tail.

(define (fib n)
  (if (<= n 1)
      n
      (+ (fib (- n 1))
         (fib (- n 2)))))

; In this exercise, it is OK to keep "if (<= n 1)" in direct style, no need to change.
; Just focus on the minuses, the plus, and the recursive calls.

; Answer:

(define (fib-cps n cont)
  (if (<= n 1)
      "unimplemented"
      "unimplemented"))

; Q3: A higher-order function.

(define (f23 f)
  (+ (f 2) (f 3)))

; Note that after rewriting to CPS, we expect to receive a CPS function too, e.g.,
; a user that intended (f23 sin) now writes (f23 sin-cps cont)

; Answer:

(define (f23-cps f-cps cont)
  "unimplemented")
