#lang racket ; CSC324 — 2020F — Lab 3

; This lab uses higher-order functions to create one specific class: a Point class.
; In lecture this week we will capture this code design pattern as a macro to create a
;  general class operation, illustrating how Object-Orientation is a particular pattern
;  of higher-order function usage.

; There are eight tasks, numbered 1 — 8.

; • Testing

; The code in this section starts a “sub-module” named “test”, that imports a standard racket
;  testing framework and creates a  same!  assertion form from it.
;
; The code in a sub-module doesn't run automatically, but DrRacket runs any sub-modules
;  named “test” or “main”, after the rest of the code runs (and this behaviour is configurable).
;
; There are a few kinds of sub-module.
; The  module+  form spefically creates a sub-module that can:
;   - reference definitions outside the sub-module
;   - be defined incrementally by using  module+  multiple times (with the same module name)
;
(module+ test
  
  (require rackunit) ; import  check-equal?

  ; Let's create  same!  that allows more than two expressions to be compared for equality,
  ;  and that prints each pair of expressions before comparing them ...
  (define-syntax same!
    (syntax-rules ()
      ((same! e.1 e.2)
       (begin (writeln '(same! e.1 e.2))
              (check-equal? e.1 e.2)))
      ((same! e.1 e.2 e.3 es ...)
       (begin (same! e.1 e.2)
              (same! e.2 e.3 es ...))))))

; • The Point class constructor

; The Point class will be a function that behaves as the single constructor for “objects” of a class
;  to represent 2D points. The constructor is binary, taking values for the x and y co-ordinates.

(define Point "unimplemented")

; Start by defining Point so that it can be called as shown in the following definition so the definition
;  can run without error. Use a trivial body for Point for now, then uncomment the definition and run.

(define p.1 (Point 8 -15))

; • Point object properties

; Point will return an object as a function that can be called with the name of a “property” or a “method”.
; Since we don't care about the characters in a name, we'll use the traditional datatype for names: symbols.

; When called with the name of a property a point object will return the value of that property.

; Let's start with two properties  x  and  y ...
#;(module+ test
  
    (same! ((Point 3 4) 'x)
         
           ; 1. Replace “_” with an appropriate expression ...
           #;(_
              'x)
           ; ... so that call does ...
           (match 'x
             ('x 3)
             ('y 4))
         
           3)

    (same! (p.1 'y)
           -15))

; 2. Fill in the body of Point to implement the properties  x  and  y .

; 3. Write out the algebraic closure representing the value of ...
p.1

; 4. Add code to the body of Point to implement a property for the size of a point ...

#;(module+ test
    (same! (p.1 'size)
           (sqrt (+ (sqr 8) (sqr -15)))
           17))

; 5. Write out the algebraic closure representing the value of ...
(Point 3 4)

; • Point object methods

; A method is still a property, but whose value is a function that can be called with the method's arguments.

; The Point class has two methods  scale  and  add .

#;(define p.2 ((p.1 'scale) 2)) #;(Point 16 -30)

; Let's get a feel for what the  scale  property should return.

#;(module+ test

    ; Recall: this test is theoretical, since comparing function is ill-defined.
  
    #;(same!
     
       ((p.1 'scale) 2)
     
       ; 6. Replace “_” with a function that would create a scaled version of (Point 8 -15) ...
       (_
        2)
       ; ... so that call does ...
       (Point (* 2 8) (* 2 -15))
     
       (Point 16 -30)
       p.2))

; 7. Add code to the body of Point to implement  scale .

#;(module+ test
    (same! (((p.1 'scale) 2) 'x)
           16)
    (same! (((p.1 'scale) 3) 'y)
           -45))

; 8. Add code to the body of Point to implement  add  ...

#;(module+ test

    ; Another theoretical test.
    #;(same! ((p.1 'add) (Point -5 11))
             (Point 3 -4))
  
    (same! (((p.1 'add) (Point -5 11)) 'size)
           ((Point 3 -4) 'size)
           5))
