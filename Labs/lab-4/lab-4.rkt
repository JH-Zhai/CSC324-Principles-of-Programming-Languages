#lang racket ; CSC324 — 2020F — Lab 4

; This lab leads you through making a class macro.
; It focuses on recognizing and expressing code patterns.
; It has 8 steps.

; NOTE: Double-clicking on a parenthesis in code selects the term it encloses,
;  which is convenient for replacing, deleting or copying it.

(module+ test (require rackunit))

; First, let's see a version of the Point class whose methods can be called a bit more directly.

; It uses a special form of λ that allows for an arbitrary number of arguments.
#;(λ parameter-name ; note the lack of parentheses around parameter-name
    body ...)

(module+ test
  ; It receives the arguments as a list, so returning that returns the list ...
  (check-equal? ((λ arguments arguments) 3 2 4) '(3 2 4))
  ; We could sum the squares of the arguments ...
  (check-equal? ((λ numbers (apply + (map sqr numbers))) 3 2 4) 29)
  ; We can react to the number of arguments ...
  (check-equal? ((λ optional (if (empty? optional) "no arguments" "some arguments")))
                "no arguments")
  (check-equal? ((λ optional (if (empty? optional) "no arguments" "some arguments")) 3 2 4)
                "some arguments"))

; We'll also use the fact that quotation is an operation, unlike, for example, double-quotes for strings.
; That can be made more explicit using the form (quote _):
(module+ test
  (check-equal? (quote a-symbol) 'a-symbol)
  (check-equal? (quote (a (b c))) '(a (b c))))

; Here's a Point class.
(define (Point x y)
  (λ method-name-and-arguments
    (match method-name-and-arguments
      ((list 'size) (sqrt (+ (sqr x) (sqr y))))
      ((list 'scale factor) (Point (* factor x) (* factor y)))
      ((list 'shift dx dy) (Point (+ x dx) (+ y dy))))))

(define p (Point 3 4))
(module+ test
  (check-equal? (p 'size) 5)
  (check-equal? ((p 'scale 2) 'size) 10)
  (check-equal? ((p 'shift 2 8) 'size) 13))

; 1. Write down the value of method-name-and-arguments for each of the calls ...
(p 'size) #;(list 'size)
(p 'scale 2) #;(list 'scale 2)
(p 'shift 6 7) #;(list 'shift 6 7)

; I'd like to be able to define the Point class by just writing:
#;(class (Point x y)
    ((size) (sqrt (+ (sqr x) (sqr y))))
    ((scale factor) (Point (* factor x) (* factor y)))
    ((shift dx dy) (Point (+ x dx) (+ y dy))))

; Similarly, I'd like to be able to write a Person class by just writing:
#;(class (Person surname given birth-year)
    ((greeted with) (string-append with " " given " " surname "."))
    ((centennial) (+ birth-year 100)))

#;(module+ test
    (check-equal? ((Person "Furler" "Sia" 1975) 'greeted "Hi") "Hi Sia Furler.")
    (check-equal? ((Person "Lovelace" "Ada" 1852) 'centennial) 1952))

; 2. Define Person by mimicing the implementation of Point, which you should start with and modify.
; Pay attention to exactly which parts you change, and which parts stay the same.
#;(define (Point x y)
    (λ method-name-and-arguments
      (match method-name-and-arguments
        ((list (quote size)) (sqrt (+ (sqr x) (sqr y))))
        ((list (quote scale) factor) (Point (* factor x) (* factor y)))
        ((list (quote shift) dx dy) (Point (+ x dx) (+ y dy))))))

#;(define (Person surname given birth-year)
    (λ method-name-and-arguments
      (match method-name-and-arguments
        ((list (quote greeted) (quote with)) (string-append with " " given " " surname ".") )
        ((list (quote centennial)) (+ birth-year 100) ))))



; 3. Fill in the blanks in the following (multi-line) sentence.
; The Point specification  
#;(class (Point x y)
    ((size) (sqrt (+ (sqr x) (sqr y))))
    ((scale factor) (Point (* factor x) (* factor y)))
    ((shift dx dy) (Point (+ x dx) (+ y dy))))
; has the form
#;(class constructor
    method
    ...)
; where  “constructor”  represents
#;(Point x y)
; and  “method ...”  represents the three terms
#;_((size) (sqrt (+ (sqr x) (sqr y))))
#;_((scale factor) (Point (* factor x) (* factor y)))
#;_((shift dx dy) (Point (+ x dx) (+ y dy)))

; 4. Fill in the blanks in the following (multi-line) sentence.
; Each “method” has the form
#;(header body)
; where the three “header”s are
#;_(size)
#;_(scale factor)
#;_(shift dx dy)
; and the three “body”s are
#;_(sqrt (+ (sqr x) (sqr y)))
#;_(Point (* factor x) (* factor y))
#;_(Point (+ x dx) (+ y dy))

; 5. Fill in the blanks in the following (multi-line) sentence.
; Each “header” has the form
#;(method-name parameter ...)
; where the three “method-name”s are
#;_size
#;_scale
#;_shift
; and the three sequences of zero-or-more “parameter”s are
; ___
; ___factor
; ___dx dy

; 6. Produce a detailed pattern for code of the above form by simply replacing
;   · “method” with “(header body)”
;   · “header” with “(method-name parameter ...)”
; in
#;(class constructor
    ((method-name parameter ...) body)
    ...)
; and put that into the Pattern part of the class macro.

#;(define-syntax class
    (syntax-rules ()
      (; Pattern
       (class constructor
         ((method-name parameter ...) body)
         ...)
       ; Template
       (define constructor
         (λ method-name-and-arguments
           (match method-name-and-arguments
             ((list (quote method-name) parameter ...) body)...))))))

; 7. Now work on the Template, describing where to put the parts of the specification to produce the implementation.
; Since constructor represents (Point x y), replace (Point x y) with just constructor.
; Since method-name represents each of size, scale, and add, replace each of those with method-name.
; Do the equivalent for body.
; Do the equivalent for parameter ... .

; 8. The three methods should now be identical in the Template, so remove two of them,
;  and insert “...” to indicate those should be repeated as many times as necessary to use up
;  the method-names, bodies, and parameter sequences.
