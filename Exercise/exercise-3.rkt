;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "2019-fall-reader.rkt" "csc104")((modname exercise-3) (compthink-settings #hash((prefix-types? . #f))))
; CSC324 — 2020F — Exercise 3

; This exercise defines some higher-order functions that produce functions, and then uses those
;  to define some new functions.

; In particular, producing functions by combining functions via higher-order-functions is called
;  “point free” style: it avoids using a header to specify what the function does to a "point",
;  i.e. to an argument, directly.

; It then defines a function to transform values representing expressions, preparing us to do
;  more sophisticated code transformations (which will be made more convenient later with
;  racket and Haskell's “pattern matching”).

(require (only-in racket provide))
(provide checks-first
         if? or? and? fun?
         Or
         propositional?
         racket)

; • checks-first

; checks-first takes a value and produces a unary predicate that checks whether a value
;  is a non-empty list that starts with the value ...
(true! ((checks-first "eel") (list "eel" "emu" "elf")))
(false! ((checks-first "elf") (list "eel" "emu" "elf")))
(false! ((checks-first "emu") "emu"))
(false! ((checks-first "eel") (list)))

; ★ Define checks-first ...

(define (checks-first e)((fun v)(and (list? v)  (not (empty? v)) (same? (first v) e ))))


; • if? or? and? fun?

; These functions check whether a value is a list starting with "if", "or", "and", or "fun", respectively.

(false! (if? "if"))
(true! (if? (list "if" "not" "now" "when")))
(false! (if? (list "and" "if" "not")))

(same! (map or? (list (list "or" "an" "ge")
                        (list "if" "fy")
                        (list "or")))
         (list #true #false #true))

(same! (select and? (list (list) (list "and" "so") (list "on")
                            (list "and") "on" "and" "on"))
         (list (list "and" "so") (list "and")))

(true! (fun? (list "fun" "knee")))

; ★ Define the four functions using checks-first, WITHOUT using the expression forms ...
#;(define (function-name parameter-name ...) body-expression) ; ... nor  ...
#;((fun parameter-name ...) body-expression)

(define if? (checks-first "if"))
(define and? (checks-first "and"))
(define or? (checks-first "or"))
(define fun? (checks-first "fun"))


; • Or

; The function Or takes two unary predicates and produces a unary predicate for their disjunction ...
(same! (select (Or ((fun n) (zero? (remainder n 2)))
                     ((fun n) (zero? (remainder n 3))))
                 (range 10))
         (list 0 2 3 4 6 8 9))

; ★ Define Or ...

(define (Or a b) ((fun n) (or (a n) (b n))))


; • propositional?

; The function propositional? takes a value and determines whether it's a list that starts with "and" or "or" ...
(same! (select propositional?
                 (join (list (list "or" "an" "ge") (list "if" "fy") (list "or"))
                       (list (list) (list "and" "so") (list "on") (list "and") "on" "and" "on")))
         (list (list "or" "an" "ge")
               (list "or")
               (list "and" "so")
               (list "and")))

; ★ Define propositional? WITHOUT using the expression forms ...
#;(define (function-name parameter-name ...) body-expression) ; ... nor  ...
#;((fun parameter-name ...) body-expression)

(define (propositional? v) ((Or (checks-first "and") (checks-first "or")) v))


; • racket

; Let's first define an alias for the list function, to make it less distracting for complicated nested lists ...
(define : list)

; And, although the functions we've used so far are sufficient, you might sometimes prefer using
;   element  and/or  prepend  (although make sure you could define them yourself) ...
(same! (map ((fun i) (element (list "ape" "bat" "cat") i))
            (range 3))
       (list "ape" "bat" "cat"))
(same! (prepend "ape" (list "bat" "cat"))
       (join (list "ape") (list "bat" "cat")))

; The function racket translates an expression from the csc104 language, represented as a nested list of strings,
;  to a racket equivalent (also as a nested list of strings), for a subset of the csc104 language.
(same! (racket (: "if" (: "<" "123" "x") (: (: "fun" "y" "z") "456")
                    "else" (: "and" "y" (: "or" (: "f" "z") (: "g" "789" "a") "b"))))
         (: "if" (: "#%app" "<" "123" "x") (: "λ" (: "y" "z") "456")
            (: "and" "y" (: "or" (: "#%app" "f" "z") (: "#%app" "g" "789" "a") "b"))))

; In general, ...

#;(racket (: "if" condition consequent "else" alternative)) ; ... produces ...
#;(: "if" (racket condition) (racket consequent) (racket alternative))
;  — and we'll assume that any list argument starting with "if" has the form shown

#;(racket (: "and" e.0 e.1 ...)) ; ... produces ...
#;(: "and" (racket e.0) (racket e.1) ...)
#;(racket (: "or" e.0 e.1 ...)) ; ... produces ...
#;(: "or" (racket e.0) (racket e.1) ...)

#;(racket (: (: "fun" id ...) e)) ; ... produces ...
#;(: "λ" (: id ...) (racket e))
;  — and we'll assume that any list argument starting with a list that starts with "fun" has the form shown

#;(racket (: e.0 e.1 ...)) ; ... if not handled by any of the previous produces ...
#;(: "#%app" (racket e.0) (racket e.1) ...)

#;(racket e) ; ... if not handled by any of the previous just produces ...
#;e

; ★ Define racket.

; Use if? propositional? and fun? appropriately in your implementation.

(define (racket lst)
  ( if (not (list? lst)) lst
       else ( if (and (propositional? lst) (> (length lst) 1 )) (prepend (element lst 0) (map racket (rest lst)))
                 else ( if (and ((checks-first "if") lst) (same? (element lst 3) "else") (same? 5 (length lst))) (prepend "if" (map racket (:(element lst 1) (element lst 2) (element lst 4))))
                           else ( if (and  (list? (element lst 0))  (same? "fun" (element (element lst 0) 0)) (> (length (element lst 0)) 1)   (same? 2 (length lst))) (: "λ"  (rest (element lst 0)) (racket (element lst 1)))
                                     else (prepend "#%app"  (map racket lst))
                                     )
                           )
                 )
       )
 )
