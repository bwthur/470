#lang racket
(define elementAt
  (lambda(lst index)
    (cond
      ((not (list? lst)) "This is not a list")
      ((null? lst) "This is an empty list or index is out of bounds")
      ((equal? index 0) (car lst))
      (else (elementAt (cdr lst) (- index 1)))
      )
    )
  )

(provide (all-defined-out))