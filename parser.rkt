#lang racket

(require "utility.rkt")

(define neo-parser
  (lambda (neo-code)
    (cond
      ((null? neo-code) '())
      ((number? neo-code) (list 'num-exp neo-code))
      ((symbol? neo-code) (list 'var-exp neo-code))
       ((equal? (car neo-code) 'bool)
        (if (equal? (length neo-code) 3)
            (list 'bool-exp (cadr neo-code) (neo-parser (caddr neo-code)) '())
        (cons 'bool-exp (cons (cadr neo-code) (map neo-parser (cddr neo-code))))))
      ((equal? (car neo-code) 'math)
       (list 'math-exp (cadr neo-code)
             (neo-parser (caddr neo-code))
             (neo-parser (cadddr neo-code))))
      ((equal? (car neo-code) 'ask)
       ;HW-10 Fixing Bug, change from list to cons to combine the elements
       ;before: (list 'ask-exp
       (cons 'ask-exp
             (map neo-parser (cdr neo-code))))
      ((equal? (car neo-code) 'function)
       (list 'func-exp
             (list 'params (cadr neo-code))
             (list 'body-exp (neo-parser (caddr neo-code)))))
      ((equal? (car neo-code) 'call)
       (list 'app-exp
             (neo-parser (cadr neo-code))
             (neo-parser (caddr neo-code))))
      (else (map neo-parser neo-code))
      )
    )
  )


(provide (all-defined-out))