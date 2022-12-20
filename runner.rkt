#lang racket

(require "utility.rkt")

;resolve value from variable environment
(define resolve
  (lambda (environment varname)
    (cond
      ((null? environment) #false)
      ((equal? (caar environment) varname) (cadar environment))
      (else (resolve (cdr environment) varname))
      )
    )
  )

(define extend-env
  (lambda (list-of-varname list-of-value env)
    (cond
      ((null? list-of-varname) env)
      ((null? list-of-value) env)
      (else (extend-env (cdr list-of-varname) (cdr list-of-value)
       (cons (list (car list-of-varname)
                   (car list-of-value))
             env)))
      )
    )
  )

(define run-neo-parsed-code
  (lambda (parsed-code env)
    (cond
      ((null? parsed-code) '())
      ((equal? (car parsed-code) 'num-exp)
       (cadr parsed-code))
      ((equal? (car parsed-code) 'var-exp)
       (resolve env (cadr parsed-code)))
      ((equal? (car parsed-code) 'bool-exp) (run-bool-parsed-code parsed-code env))
      ((equal? (car parsed-code) 'math-exp) (run-math-parsed-code parsed-code env))
      ((equal? (car parsed-code) 'ask-exp)
       (if (run-neo-parsed-code (cadr parsed-code) env)
           (run-neo-parsed-code (caddr parsed-code) env)
           (run-neo-parsed-code (cadddr parsed-code) env)))
      ((equal? (car parsed-code) 'func-exp)
       (run-neo-parsed-code (cadr (caddr parsed-code)) env))
      (else (run-neo-parsed-code
             (cadr parsed-code)
             (extend-env
              (cadr (cadr (cadr parsed-code)))
              (map (lambda (exp) (run-neo-parsed-code exp env)) (caddr parsed-code))
              env)
             )
            )
      )
    ) 
  )

;runs boolean parsed code
(define run-bool-parsed-code
  (lambda(parsed-code env)
    (run-bool-exp (cadr parsed-code)
                  (run-neo-parsed-code (caddr parsed-code) env)
                  (run-neo-parsed-code (cadddr parsed-code) env))
  )
)

;HW 12 ↓

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

;runs math parsed code
(define run-math-parsed-code
  (lambda(parsed-code env)
           (run-math-exp (cadr parsed-code)
                     (run-neo-parsed-code (caddr parsed-code) env)
                     (run-neo-parsed-code (cadddr parsed-code) env))
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


(provide (all-defined-out))