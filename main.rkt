#lang racket
(require "utility.rkt")
(require "runner.rkt")
(require "parser.rkt")



;(define sample-code '(call (function (r) (local-vars ((p 100)) (math / r p)) ) (a)))

(define env '((global (a 1)) (b 2) (c 5)))

;(define sample-code '(block (assign x 8) (print x)))
(define sample-code '(block (print a) (assign x 8) (assign y (math * x 2)) (print y) (assign z (math + b y)) (print z)))
(displayln (neo-parser sample-code))
(define parsed-neo-code (neo-parser sample-code))
(run-neo-parsed-code parsed-neo-code env)