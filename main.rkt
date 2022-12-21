#lang racket
(require "utility.rkt")
(require "runner.rkt")
(require "parser.rkt")

(define env '((global (a 1) (b 2) (c 5))))

;(define sample-code '(call (function (r) (local-vars ((p 100)) (math / r p)) ) (a)))
(define sample-code '(local-vars ((p c)) (math / a p)))
(displayln (neo-parser sample-code))
(define parsed-neo-code (neo-parser sample-code))
(run-neo-parsed-code parsed-neo-code env)