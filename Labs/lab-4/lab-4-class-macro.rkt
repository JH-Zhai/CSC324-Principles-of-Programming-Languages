#lang racket ; CSC324 — 2020F — Lab 4 — class Solution

(define-syntax class
  (syntax-rules ()
    (; Pattern
     (class constructor
       ((method-name parameter ...) body)
       ...)
     ; Template
     (define constructor
       (λ method-name-and-arguments
         (match method-name-and-arguments
           ((list (quote method-name) parameter ...) body)
           ...))))))

; Click the Macro Stepper, to see this code get rewritten.

(class (Point x y)
  ((size) (sqrt (+ (sqr x) (sqr y))))
  ((scale factor) (Point (* factor x) (* factor y)))
  ((shift dx dy) (Point (+ x dx) (+ y dy))))

(class (Person surname given birth-year)
  ((greeted with) (string-append with " " given " " surname "."))
  ((centennial) (+ birth-year 100)))

; And then Run to see that the rewritten code works as expected.

(module+ test (require rackunit))

(define p (Point 3 4))

(module+ test
  (check-equal? (p 'size) 5)
  (check-equal? ((p 'scale 2) 'size) 10)
  (check-equal? ((p 'shift 2 8) 'size) 13))

(module+ test
  (check-equal? ((Person "Furler" "Sia" 1975) 'greeted "Hi") "Hi Sia Furler.")
  (check-equal? ((Person "Lovelace" "Ada" 1852) 'centennial) 1952))


