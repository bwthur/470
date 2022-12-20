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

(define run-math-exp
  (lambda (op num1 num2)
    (cond
      ((equal? op '+) (+ num1 num2))
      ((equal? op '-) (- num1 num2))
      ((equal? op '*) (* num1 num2))
      ((equal? op '/) (/ num1 num2))
      ((equal? op '//) (quotient num1 num2))
      ((equal? op '%) (modulo num1 num2))
      (else #false)
      )
    )
  )

(define run-bool-exp
  (lambda (op num1 num2)
    (cond
      ((equal? op '>) (> num1 num2))
      ((equal? op '<) (< num1 num2))
      ((equal? op '>=) (>= num1 num2))
      ((equal? op '<=) (<= num1 num2))
      ((equal? op '==) (= num1 num2))
      ((equal? op '!=) (not (= num1 num2)))
      ((equal? op '&&) (and num1 num2))
      ((equal? op '||) (or num1 num2))
      (else (not num1))
      )
    )
  )

(provide (all-defined-out))