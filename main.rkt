#lang racket
(require "utility.rkt")
(require "runner.rkt")
(require "parser.rkt")



;(define sample-code '(call (function (r) (local-vars ((p 100)) (math / r p)) ) (a)))

;HW-15
(define env '((global (a 1)) (b 2)))
(define sample-code '(print a))
;-----


(displayln (neo-parser sample-code))
(define parsed-neo-code (neo-parser sample-code))
(run-neo-parsed-code parsed-neo-code env)