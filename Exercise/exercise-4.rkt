#lang racket ; CSC324 — 2020F — Exercise 4

; Due Tuesday October 13th at 9PM.

(provide maybe-map
         matches)

; This exercise implements some basic pattern matching.

; • Functional Programming

; We're still practicing programming without mutation, and using higher-order functions.

; So do not use any operations nor functions that alter a variable or data-structure in-place.

; Also, do not use the syntactically loop-like forms (for/list, for/fold, etc) to process lists —
;  instead use the functions listed in sections 4.10.1, 2, 3, 4, 5, and 7 of the racket reference:
#;https://docs.racket-lang.org/reference/pairs.html

; But see the next section: the forms listed there, and familiar csc104 functions (possibly
;  with different names), are sufficient to define the two functions in this exercise.

; • Suggested Racket Forms and Functions to Use

;  The two functions in this exercise are definable fairly naturally using at most the forms ...

#;#true
#;#false

#;variable-reference

#;(function-expression argument-expression ...)

#;(and condition-expression ...)
#;(or condition-expression ...)

#;(define variable-name value-expression)

#;(if condition-expression
      consequent-expression
      alternative-expression)

#;(define (function-name parameter-name ...)
    definition ...
    body-expression)

#;(λ (parameter-name ...)
    definition ...
    body-expression)

#;(cond (condition-expression definition ...
                              result-expression)
        ...
        (else definition ...
              alternative-expression))

#;(match expression
    (pattern definition ...
             result-expression)
    ...)

; Notice that bodies of functions, and consequences of conditions in cond and patterns in match,
;  can contain a sequence of definitions of local variables before the result expression.

; Also, the csc104 functions ...
#;combine #;join #;prepend
; ... are named ...
#;apply #;append #;list*
; ... in racket.

; • Testing Framework

; See Lab 3 for a discussion of a “test sub-module”.

; This one defines assertion forms analogous to the ones in the csc104 language ...
#;(same!  e.1 e.2 e ...)
#;(true!  e.1 e ...)
#;(false! e.1 e ...)
; ... but note that the true/false forms can take more than one expression.

; It uses a macro implementation library that's a bit more sophisticated than the “syntax rules” system,
;  which allows compile-time rewritten code to be associated with source locations in the user's code.
;  So when a test fails the error highlighting (and jump location when you click the red ☒ icon) is in
;  the use of the assertion, not in the implementation of the assertion.

(module+ test
  
  (require rackunit
           syntax/parse/define)

  (define-syntax-parser same!
    ((same! e.1 e.2) (syntax/loc (attribute e.2) (check-equal? e.1 e.2)))
    ((same! e.1 e.2 e.3 es ...) (syntax (begin (same! e.1 e.2)
                                               (same! e.2 e.3 es ...)))))

  (define-syntax-parser true!
    ((true! e) (syntax/loc (attribute e) (check-equal? e #true)))
    ((true! e ...) (syntax (begin (true! e) ...))))
  
  (define-syntax-parser false!
    ((false! e) (syntax/loc (attribute e) (check-equal? e #false)))
    ((false! e ...) (syntax (begin (false! e) ...)))))

; • maybe-map

; The function maybe-map takes a binary function f and two lists l.1 and l.2 of the same length,
;  and produces a list of the results of calling f on pairs of elements from l.1 and l.2 in parallel,
;  except that if f produces #false for a pair of elements the whole function just produces #false:

#;(maybe-map f (list e.1 ...) (list e.2)) ; ... produces ...
#;(list (f e.1 e.2) ...) ;  ... unless one of the calls of f produces #false.



  (equal? (maybe-map + (list) (list))
         (list)) 

  (equal? (maybe-map (λ (t.1 t.2) (string-append t.1 "-" t.2))
                    (list "ant" "bat" "cat")
                    (list "eater" "man" "burglar"))
         (list* (string-append "ant" "-" "eater")
                (maybe-map (λ (t.1 t.2) (string-append t.1 "-" t.2))
                           (list "bat" "cat")
                           (list "man" "burglar")))
         (list "ant-eater" "bat-man" "cat-burglar"))

  (equal? (maybe-map < (list 2 4) (list 2 "three"))
         #false)

  (equal? (maybe-map < (list 3 2 4) (list 4 2 "three"))
         #false)

; Define maybe-map ...

(define (maybe-map f l.1 l.2)
  (cond[(empty? l.1)(list )]
       [(or (not (f (first l.1) (first l.2)) ) (not (maybe-map f (rest l.1) (rest l.2)) ) ) #f]
       [else (append (list (f (first l.1) (first l.2))) (maybe-map f (rest l.1) (rest l.2)))]))

; Hint : A carefully chosen set of non-redundant illustrative test cases (which we've provided)
;  is a good default guide to the conditional structure of an implementation.

; • matches

; The function matches takes a pattern and a value.

; A pattern is either:
;  - a symbol
;  - a list of patterns, none of which contain the same symbol twice anywhere

; It determines whether the value “matches” the pattern, i.e. if the symbols in the pattern
;  could be replaced with values in order to produce the value. If the pattern matches,
;  it produces a list of lists of symbol and replacement, otherwise it produces #false.



  (equal? (matches 'a 1)
         '((a 1)))

  (false? (matches '(b) 2))
  
  (false? (matches '(a b) '(1)))
  (false? (matches '(a b c) '(1 2 3 4)))
  
  (equal? (matches '(a b c) '(1 2 3))
         '((a 1) (b 2) (c 3)))
  (false? (matches '(a (b) c) '(1 2 3)))

  (equal? (matches '(a (b) ((c) d)) '((1 2) (3) (((4 5)) 6)))
         '((a (1 2)) (b 3) (c (4 5)) (d 6)))

; Define matches ...

(define (matches p v)
  (cond [(not(list? p))  (list (list p v))]
        [(not(list? v))  #false]
        [(not(equal? (length v) (length p))) #false]
        [(and (empty? p) (empty? v)) (list )]
        [else(if (and (list? (matches (first p) (first v))) (list? (matches (rest p) (rest v))))(append (matches (first p) (first v)) (matches (rest p) (rest v))) #false)]))

; Hint : for each false! assertion, insert another expression that illustrates why it should be false
;  (using one or both of the arguments).
; Hint : for the other assertions, insert at least one expression that builds the result meaningfully
;  (using the arguments, or parts of them).
