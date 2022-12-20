#lang racket

(require "utility.rkt")
(require "runner.rkt")
(require "parser.rkt")

(define env '((a 1) (b 2) (c 5)))
(resolve env 'a)

(define sample-code '(call (function () (ask (bool != a b) (math - a b) (math + a b))) (a)))
(display (neo-parser sample-code))
(define parsed-neo-code (neo-parser sample-code))
(run-neo-parsed-code parsed-neo-code env)