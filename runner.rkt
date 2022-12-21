#lang racket

(require "utility.rkt")

;resolve value from variable environment
(define resolve_scope
  (lambda (scope varname)
    (cond
      ((null? scope) #false)
      ((equal? (caar scope) varname) (cadar scope))
      (else (resolve_scope (cdr scope) varname))
      )
    )
  )

(define resolve_env
  (lambda (environment varname)
    (cond
      ((null? environment) #false)
      ((null? (car environment)) (resolve_env (cdr environment) varname))
      ((equal? 'global (car (car environment))) (resolve_scope (cdr (car environment)) varname))
      (else (let ((resolved_result (resolve_scope (car environment) varname)))
              (if (equal? resolved_result #false)
                  (resolve_env (cdr environment) varname)
                  resolved_result
                  )
              )
       )
      )
    )
  )

(define extend-scope
  (lambda (list-of-varname list-of-value scope)
    (cond
      ((null? list-of-varname) scope)
      ((null? list-of-value) scope)
      (else (extend-scope (cdr list-of-varname) (cdr list-of-value)
       (cons (list (car list-of-varname)
                   (car list-of-value))
             scope)))
      )
    )
  )

(define push_scope_to_env
  (lambda (list-of-varname list-of-value env)
    (let ((new_scope (extend-scope list-of-varname list-of-value '()))
          (pop_off_env (pop_env_to_global_scope env)))
      (cons new_scope pop_off_env)
      )
    )
  )


;removes scopes that are on top of global scope
(define pop_env_to_global_scope
  (lambda (env)
    (cond
      ((null? env) #false)
      ((equal? (length env) 1)
       (if (equal? (car (car env)) 'global) env
           #false))
      (else (pop_env_to_global_scope (cdr env)))
      )
    )
  )

(define extend_local_scope
  (lambda (list-of-varname list-of-value env)
    (cond
      ((null? env) #false)
      ((equal? (caar env) 'global) (push_scope_to_env list-of-varname list-of-value env))
      (else (cons (extend-scope list-of-varname list-of-value (car env))
                  (pop_env_to_global_scope env)))
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
       (resolve_env env (cadr parsed-code)))
      ((equal? (car parsed-code) 'bool-exp) (run-bool-parsed-code (cdr parsed-code) env))
      ((equal? (car parsed-code) 'math-exp)
       (run-math-exp (cadr parsed-code)
                     (run-neo-parsed-code (caddr parsed-code) env)
                     (run-neo-parsed-code (cadddr parsed-code) env)))
      ((equal? (car parsed-code) 'ask-exp)
       (if (run-neo-parsed-code (cadr parsed-code) env)
           (run-neo-parsed-code (caddr parsed-code) env)
           (run-neo-parsed-code (cadddr parsed-code) env)))
      ((equal? (car parsed-code) 'func-exp)
       (run-neo-parsed-code (cadr (caddr parsed-code)) env))
      ((equal? (car parsed-code) 'let-exp)
       (run-let-exp parsed-code env))
      ((equal? (car parsed-code) 'print-exp)
       (run-print-exp (cadr parsed-code) env))
      ((equal? (car parsed-code) 'assign-exp)
       (run-assign-exp (elementAt parsed-code 1)
                       (run-neo-parsed-code (elementAt parsed-code 2) env)
                       env))
      ((equal? (car parsed-code) 'block-exp)
       (run-block-exp (cdr parsed-code) env))
      (else (run-neo-parsed-code
             (cadr parsed-code)
             (push_scope_to_env (cadr (cadr (cadr parsed-code)))
                                (map (lambda (exp) (run-neo-parsed-code exp env)) (caddr parsed-code))
                                env
                                )
             )
         )            
      )
    ) 
  )
  


;run bool parsed code
(define run-bool-parsed-code
  (lambda(parsed-code env)
    (let ((op (elementAt parsed-code 0))
           (num1 (run-neo-parsed-code (elementAt parsed-code 1) env))
           (num2 (run-neo-parsed-code (elementAt parsed-code 2) env)))
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

    

(define run-let-exp
  (lambda (parsed-code env)
    (let* ((new_env (cascade-update-env (elementAt parsed-code 1) env))
          (body (elementAt parsed-code 2)))
      (run-neo-parsed-code body new_env)
    )
  )
)

(define cascade-update-env
  (lambda (parsed-scope env)
    (if (null? parsed-scope) env
        (let* (
               (local-scope (if (equal? (car (car env)) 'global)
                                '()
                                (car env)))
               (global-scope-env (pop_env_to_global_scope env))

               (first-name-value-pair (car parsed-scope))
               (new-local-scope (cons
                                 (list
                                  (car first-name-value-pair)
                                  (run-neo-parsed-code (cadr first-name-value-pair) env))
                                 local-scope))
               (new_env (cons new-local-scope global-scope-env))
               )
          (cascade-update-env (cdr parsed-scope) new_env)
          )
      )
    )
  )

(define run-print-exp
  (lambda (parsed-code env)
    (display (string-append "**screen** " (number->string
                                           (run-neo-parsed-code parsed-code env))))
    )
  )

(define run-assign-exp
  (lambda (varname value env)
    (cond
      ((null? env) #false)
      ((equal? (caar env) 'global)
       (cons (list (list varname value)) env))
      (else (let*
          ((new-local-scope (cons (list varname value) (car env)))
           (under-env (cdr env)))
        (cons new-local-scope under-env))
      )
      )
    )
  )

(define run-block-exp
  (lambda (parsed-list-exp env)
    (cond
      ((null? parsed-list-exp) '())
      ((equal? (caar parsed-list-exp) 'assign-exp)
       (run-block-exp
        (cdr parsed-list-exp)
        (run-assign-exp
         (cadr (car parsed-list-exp))
         (run-neo-parsed-code (elementAt (car parsed-list-exp) 2) env)
         env)))
      (else
       (let ((return (run-neo-parsed-code (car parsed-list-exp) env)))
         (if (void? return) (run-block-exp (cdr parsed-list-exp) env)
                         (cons return (run-block-exp (cdr parsed-list-exp) env))
                         )
         )
      )
    )
  )
)

(provide (all-defined-out))