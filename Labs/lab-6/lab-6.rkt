#lang racket

#; OOP Self

(module+ test (require rackunit))

; This lab extends our model of OOP to cover the "self" parameter in Python and
; OCaml, equivalently "this" in Java, C++, etc. An importance of "self" is that
; it is how a method can call another method of the same object.
;
; It is simpler to follow Python in insisting that every method implementation
; takes an explicit self parameter as the 1st parameter. (But this doesn't
; affect users of methods.)  Here is an example of what I mean. In Python
; pseudocode:
;
; class Toy:
;   def __init__(self, x):
;     ...
;   def call_offset(self, dx):
;     self.offset(dx)  # use self to call my other method
;   def offset(self, dx);
;     ...
;
; In Racket after you define the "class" macro, I can write this:

#;(class (Toy x)
    ((call-offset self dx) (self 'offset dx))
    ((offset self dx) (+ x dx)))

; Note that each method implementation takes a self parameter, and may use it to
; call another method.
;
; The idea is that your macro expands that to:

#;(define (Toy x)
    (define the-object
      (λ method-call
        (match method-call
          [(list (quote call-offset) dx) (let ([self the-object]) (self 'offset dx))]
          [(list (quote offset) dx) (let ([self the-object]) (+ x dx))])))
    the-object)

; In fact here is a handwritten version so you can test it:

(define (Toy-handwritten x)
  (define the-object
    (λ method-call
      (match method-call
        [(list (quote call-offset) dx) (let ([self the-object]) (self 'offset dx))]
        [(list (quote offset) dx) (let ([self the-object]) (+ x dx))])))
  the-object)

; In words:
;
; 1. As before, create a lambda closure that knows how to dispatch method calls.
;    That will be the new object.
;
; 2. But we need to refer to it, so give it a local name, say "the-object"
;    (arbitrary), when we're creating it.
;
; 3. Method code such as call-offset's (self 'offset dx) must be put inside a
;    scope that says "self means the-object", so translate to this code:
;
;    (let ([self the-object]) (self 'offset dx))
;
; 4. Can now return the-object to the user.
;
; Yes this is self-reference. Yes this is recursion. If you understand OOP, you
; understand recursion, in fact the most knotty kind, you just didn't realize
; it.  >:)

; Here is a little test and usage example:

(define toy-handwritten-example (Toy-handwritten 4))

(module+ test
  (check-equal? 5 (toy-handwritten-example 'call-offset 1))
  ; Note that the syntax for calling a method is as before.
  )

; As in Python, the name of "self" is arbitrary, I can use "this" instead, I
; just have to be consistent:

#;(class (Toy2 x)
    ((call-offset this dx) (this 'offset dx))
    ((offset this dx) (+ x dx)))

; That expands to:

#;(define (Toy2 x)
    (define the-object
      (λ method-call
        (match method-call
          [(list (quote call-offset) dx) (let ([this the-object]) (this 'offset dx))]
          [(list (quote offset) dx) (let ([this the-object]) (+ x dx))])))
    the-object)

; Implement the required macro "class".

(define-syntax class
  (syntax-rules ()
    [(class constructor
       ((method-name self-name parameter ...) body)
       ...)
     "unimplemented"
     ]))

; Uncomment these use cases and tests when you're ready:

#;(class (Toy x)
  ((call-offset self dx) (self 'offset dx))
  ((offset self dx) (+ x dx)))

#;(define toy-example (Toy 4))

#;(module+ test
  (check-equal? 5 ((Toy 4) 'call-offset 1)))

#;(class (Toy2 x)
  ((call-offset this dx) (this 'offset dx))
  ((offset this dx) (+ x dx)))

#;(define toy2-example (Toy2 4))

#;(module+ test
  (check-equal? 5 ((Toy2 4) 'call-offset 1)))
