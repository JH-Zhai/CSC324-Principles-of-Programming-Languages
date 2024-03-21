#lang racket

#; Lazy List

; CSC324 — 2020F — Exercise 5
; Due Sunday November 1, 9PM

(provide make-gs my-stop)

; This exercise practices a simple case of making and using lazy lists.

; Setup
; -----

; Lazy list definitions from lectures:

(define-syntax list*
  (syntax-rules ()
    ((list* first-expression rest-expression)
     (list first-expression (λ () rest-expression)))))

(require (prefix-in strict: racket))

(define (rest lz) ((second lz)))

(define (unfold f seed) (list* seed (unfold f (f seed))))

(define (n-of lz n) (if (zero? n) '() (strict:list* (first lz) (n-of (rest lz) (sub1 n)))))

; (define (map f lz) (list* (f (first lz)) (map f (rest lz))))


; Preamble
; --------

; Newton's method for solving an equation such as
;   x^10 + x - 31 = 0
; involves:
;
; 0. Define f(x) = x^10 + x - 31
;
;    Define g(x) = x - f(x)/f'(x)
;                = x - (x^10 + x - 31)/(10*x^9 + 1)
;
; 1. Pick an initial guess x0. Compute the sequence
;    x0, g(x0), g(g(x0)), g(g(g(x0))), ...
;    Note that the unfold function above does this.
;
; 2. Go through that sequence until it "converges". The number you get when it
;    "converges" is the answer.
;
;    "converges" can be one of many criteria. In this exercise, we use: when two
;    consecutive numbers are within 0.0001.
;
;    But note that other criteria are possible.


; Question 1
; ----------

; Implement point #1: With the help of unfold, define function make-gs so that
; (make-gs x0) returns the infinite list x0, g(x0), g(g(x0)), ...

(define (make-gs x0) (unfold (λ (x) (- x (/ (- (+ (expt x 10) x) 31) (+ (* (expt x 9) 10) 1)))) x0))

; As a simple test, (n-of (make-gs 3.0) 5) should give these numbers
; approximately:
;
; 3
; 2.7001
; 2.4305
; 2.1884
; 1.9721
;
; (If you entered 3 instead of 3.0, you would get rational numbers instead of
; floating-point numbers, they would have huge denominators, your screen would
; be flooded and you would be scared :) )


; Question 2
; ----------

; Implement point #2: Define function my-stop so that (my-stop lz) goes through
; the lazy list lz, look for the first time two consecutive numbers x, y are
; within 0.0001, i.e., |x - y| <= 0.0001, and return y.
;
; The function "rest" above in the Setup section can help. And don't forget that
; "first" works for our lazy lists too.

(define (my-stop lz) (if (not (> (abs (- (first lz) (first (rest lz)))) 0.0001)) (first (rest lz)) (my-stop (rest lz))))

; As a simple test, here is a useful input list, it stands for the (finite)
; sequence 3.0, 2.5, 2.0, 2.00009, 10.0, 11.0, 11.00009. (my-stop sample-lz)
; should return 2.00009.

(define sample-lz
  (list* 3.0 (list* 2.5 (list* 2.0 (list* 2.00009 (list* 10.0 (list* 11.0 (list* 11.00009 '()))))))))

; If all goes well, (my-stop (make-gs 3.0)) returns one root of the
; equation. It's about 1.403.
;
; If you use -3.0 instead of 3.0, you get the other root, about -1.416.


; Parting thought
; ---------------
;
; Lazy lists used this way are good at de-coupling the generator (e.g., make-gs)
; from the searcher (e.g., my-stop).
; 
; For example, if you want to solve a different equation, you can reuse my-stop,
; just give it something other than make-gs.
;
; If you want to change the "converge" criterion, you can reuse make-gs, just
; pass it to something other than my-stop.
;
; This is a toy example, but the technique can be extrapolated to bigger cases.
