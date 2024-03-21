#lang racket

#; OOP

; CSC324 — 2020F — Lab 5 — class, mutable attributes, closure


;;;;;;;
; Setup
;;;;;;;

; The macro for defining classes again:
(define-syntax class
  (syntax-rules ()
    (; Pattern
     (class constructor
       ((method-name parameter ...) body)
       ...)
     ; Template
     (define constructor
       (λ method-call
         (match method-call
           [(list (quote method-name) parameter ...) body]
           ...))))))

; I define a toy counter class with two methods inc and get (both methods
; parameterless). Interestingly, inc treats the constructor parameter as a
; mutable variable! Will it succeed? If so, what will it really do?
(class (Counter state)
  ((inc) (set! state (+ 1 state)))
  ((get) state))

; Let me create some variables and some Counter objects to test what happens:
; 0. I create a variable n and initialize it to 10.
; 1. I create a counter c1, give n as constructor parameter.
; 2. I create a counter c2, give n as constructor parameter too.
(define n 10)
(define c1 (Counter n))
(define c2 (Counter n))


; And now questions for you:

;;;;;;;;;;;;
; Question 1
;;;;;;;;;;;;

; Right after the above initializations, if we execute these commands:

(set! n 42)
(c1 'inc)
(c1 'inc)
(c2 'inc)

; What will be the new states after? I.e.,
; 0. What is the value of n now?
; 1. (c1 'get) gives what value now?
; 2. (c2 'get) gives what value now?
; I encourage you to first make your best guess unaided, then ask the computer
; for answers.

; The next questions combine the last lab and the environment-closure model to
; explain the answers.


;;;;;;;;;;;;
; Question 2
;;;;;;;;;;;;

; What does the computer rewrite
#; (class (Counter state) etc)
; to? Fill in the blanks:

#; (define _(Counter state)_
     (λ method-call
        (match method-call
          [(list 'inc)_(set! state (+ 1 state))______]
          [(list 'get)__state_____])))

; Another way to put it:

#; (define Counter
     (λ _(state)______
       (λ method-call
          (match method-call
            [(list 'inc)_(set! state (+ 1 state))______]
            [(list 'get)__state_____]))))

; Note that Counter becomes a function, "state" becomes a parameter, so local
; variable. The function goes on to make and return a closure that contains that
; local variable. The following question will make this very graphical.


;;;;;;;;;;;;
; Question 3
;;;;;;;;;;;;

; Draw the environment-closure memory model picture that results after these
; creations:
#; (define n 10)
#; (define c1 (Counter n))
#; (define c2 (Counter n))

; Note that it means we now have 3 independent mutable variables:
; 0. top-level n
; 1. c1 is a closure that has a local "state"
; 2. c2 is another closure that has another local "state"
; So now you can modify each, without affecting the others.


;;;;;;;;;;;;;;;;;;
; Parting thoughts
;;;;;;;;;;;;;;;;;;

; In the previous lab, the Point and Person objects are known as "immutable
; objects". E.g., when the scale method scales up x and y, it does so by
; creating a brand new object for the new coordinates, leaving the original
; object with the original coordinates.

; This lab shows what you can do if you want mutable objects. It looks obvious
; in hindsight, you just use the same framework, your methods just go ahead use
; set! on the attributes. This lab explains why it can be that simple.
