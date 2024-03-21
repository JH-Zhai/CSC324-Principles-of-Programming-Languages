#lang racket

#; Q2 bad implementation

(define (dfs tree k suffix-fn) (helper-enfun (helper-dfs tree k) suffix-fn))

;I defined two helper functions below, the first one uses DFS to find the desired nodes at level k, the second one take the list of nodes to generate the λ from

(define (helper-dfs lz k)
  (let ((wanted '()))
    (cond
      [(= k 0) (list (first lz))]
      [(empty? ((second lz))) wanted]
      [else (for ([child ((second lz))])
              (set! wanted (append wanted (helper-dfs child (sub1 k)))))
            wanted]
      )
    )
  )

(define (helper-enfun lst suffix-fn)
  (if (= 1 (length lst)) (λ () (list (first lst) suffix-fn))
      (λ () (list (first lst) (helper-enfun (rest lst) suffix-fn)))
      )
  )