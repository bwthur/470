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
    ;((a (num-exp 7)) (b (var-exp a)) (x (var-exp b))) > ((a 7) (b 7) (x 7))
    ;(a (num-exp 7)) -> (a 7) < (list (car code) (run-neo-parsed-code (cadr code) env)))
    ;update map with finished cascade-update-env
    (let* ((resolved-var-list (map (lambda (pair)
                                     (list (car pair) (run-neo-parsed-code (cadr pair) env)))
                                   (elementAt parsed-code 1)))
           (list-of-names (getVarnames (elementAt parsed-code 1)))
           ;list-of-values = ((num-exp 7) (var-exp a) (var-exp b))
           ;body = (math + (var-exp x) (var-exp a))
          (list-of-values (getValues resolved-var-list))
          (new_env (extend_local_scope list-of-names list-of-values env))
          ;new variables will be added to the local scope
          (body (elementAt parsed-code 2)))
    (run-neo-parsed-code body new_env)
    )
  )
)
(provide (all-defined-out))