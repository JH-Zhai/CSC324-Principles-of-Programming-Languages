#lang racket

; The questions are in exercise-6.txt.

(require racket/control)

(module+ test (require rackunit))

(define example-0
  (list "hello"
        (reset (+ 100
                  (shift k (list (k 3) (k 7)))
                  10))
        "bye"))

(module+ test
  (check-equal? example-0
                (list "hello" (list 113 117) "bye")))

; Q1

(define (green? person)
  (string=? person "Grinch"))

(define someone-green
  (reset (green? (shift k (or (k "Albert")
                              (k "Grinch"))))))

(module+ test
  (check-true someone-green))

; Q2

(define lzlist
  (reset (shift f (list 3 (λ () (f ""))))
         (shift g (list 1 (λ () (g ""))))
         '()))

(module+ test
  (check-equal? 3 (first lzlist))
  (check-equal? 1 (first ((second lzlist)))))

(define (check x) ((displayln(first x)) (displayln(first ((second x)))) (displayln((second ((second x)))))))
