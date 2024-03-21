;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "2019-fall-reader.rkt" "csc104")((modname exercise-2) (compthink-settings #hash((prefix-types? . #f))))
; CSC324 — 2020F — Exercise 2

; Requirements :
;   1. Read all the comments.
;   2. Complete the starred (★) tasks, obeying the restrictions, and also
;        uncomment the commented-out assertions as you proceed.

; This gets an operation from racket ...
(require (only-in racket/base provide))
; ... that lets us explicitly export the functions and variables you'll define ...
(provide every?
         n-of
         values has-value-for?
         sub)


; • every? : a universal-quantification function •

; Purpose of this section : review standard list-processing recursion, and implement a higher-order function.

; Restrictions : do not use the language's higher-order functions, since that would defeat the purpose of this section.

; English description of  every? : for a unary predicate and a list, determine whether every element satisfies the predicate.

(true! (every? even? (range 8 15 2)))
(false! (every? ((fun e) (same? e "cat")) (list "cat" "dog" "cat")))

; ★ Write an example/goal assertion for  every? , where the second argument is a natural base case.

(true! (every? number? (list )))


(same! (every? even? (list 8 10 12 14))

         ; ★ Insert an expression, that includes the following three expressions as sub-expressions ...
         #;every? #;8 #;(list 10 12 14)
         ; ... and illustrates why the result is ...
         (and (even? 8) (every? even? (list 10 12 14)))
         #true)


; ★ Define  every?  ...

(define (every? a b)
  (if (empty? b) #true
      else (and (a (first b)) (every? a (rest b)) ))
  )


; • n-of : a function to produce constant lists •

; Purpose of this section : use a core higher-order function with a closure.

; Restrictions : do not use recursion, since that would defeat the purpose of this section.

; English description of n-of : for a natural number n and a value, produces a list of n copies of the value.

(same! (n-of 3 "cherry")

         ; ★ Replace each “_” in ...
         #;(list (((fun i) (n-of i "cherry") ) 0) (((fun i) (n-of i "cherry") ) 1) (((fun i) (n-of i "cherry") ) 2))
         ; ... with an appropriate anonymous function.
       
         (list "cherry" "cherry" "cherry"))

; ★ Define n-of  ...

(define (n-of num v)
  (if (zero? num) (list )
      else (join (list v) (n-of (dec num) v) )))



; • has-value-for?  and  values  :  a dictionary api •

; Purpose of this section : practice using the core higher-order functions map, select, and/or combine,
;  possibly with closures, and create a dictionary api for use in the next section.

; Restrictions : do not use recursion, since that would defeat the main purpose of this section.

; A dictionary will be a list of lists where each list has two elements, for example ...
(define doctors (list (list 9 "Christopher Eccleston")
                      (list 10 "David Tennant")
                      (list 11 "Matt Smith")
                      (list 8 "Paul McGann")
                      (list 8 "John Hurt")
                      (list 12 "Peter Capaldi")
                      (list 13 "Jodie Whitaker")))


(true! (list? doctors)) ; It's a list, ...
(true! (every? list? doctors)) ; ... of lists, ...
; ... where each list has two elements ...
(same! (map length doctors) (n-of (length doctors) 2))

; ★ There's a more direct translation of “each list has two elements” : replace “_” in the following
; with an anonymous predicate that asks whether a list has length 2 ...
(true! (every? ((fun l) (same? 2 (length l))) doctors))
 

; We'll consider the first element of each list to be a key, and the second element an associated value,
;  but a key can occur multiple times to associate it with multiple values.

; English description of  values : for a dictionary and a key, produce a list of all of the key's associated values.

(same! (values doctors 11) (list "Matt Smith"))
(same! (values doctors 8) (list "Paul McGann" "John Hurt"))

; ★ Define values ...

(define (values d k)
  (map first (map rest (select ((fun l) (same? k (first l)) ) d ))))


; English description of  has-value-for? : for a dictionary and a key, determine whether the dictionary contains the key.

(true! (has-value-for? doctors 13))
(false! (has-value-for? doctors 5))
(false! (has-value-for? doctors "David Tennant"))

; ★ Define has-value-for? ...

(define (has-value-for? d k)(not (empty? (values d k))))


; • Variable Substitution •

; Purpose of this section : review recursion on nested lists, and implement basic substitution (that could be used
;  to implement variable substitution for nested lists that represent code from an algebraic language).

; Restrictions : don't use first-rest recursion on sub-lists, use a higher-order function instead.

; English description of sub : for a value and a dictionary, replace each occurrence of a key inside the value
;  with the first value for that key from the dictionary.

; An example value and dictionary ...
(define nested (list 1 (list (list 2) 3) (list (list 4 2))))
(define environment (list (list 2 "two")
                          (list 3 "three")
                          (list 2 "too")))
; ... and names for the value's elements to conveniently refer to in the next task ...
(define e.0 (first nested))
(define e.1 (second nested))
(define e.2 (third nested))

(same! (sub nested environment)

         ; ★ Insert an expression that constructs the result using the four expressions ...
         #;sub #;e.0 #;e.1 #;e.2
         (list (sub e.0 environment) (sub e.1 environment) (sub e.2 environment) )
       
         (list 1 (list (list "two") "three") (list (list 4 "two"))))

; ★ Replace all four “_”s in the following, to illustrate the two kinds of base case in the previous example ...
(define base.A (list 2)) ; ... is an example for one kind of base case, and ...
(define base.B (list 2 3)) ; ... is an  example for the other kind of base case, and ...
; ... the expected results for those examples are ...
(same! (sub base.A environment)
         
         (list "two"))
(same! (sub base.B environment)
         
         (list "two" "three"))

; ★ Define sub ...

(define (sub n d)
  (if (not (list? n))
      ( if (has-value-for? d n )   (first (values d n ) )
           else n )
      else  (map ((fun m) (sub m d) ) n )  ))










