#lang racket
(require "utility.rkt")
(require "runner.rkt")
(require "parser.rkt")

(define env '((a 1) (b 2) (c 5)))

;(define sample-code '(call (function () (ask (bool != a b) (math - a b) (math + a b))) (a)))
;(display (neo-parser sample-code))
;(define parsed-neo-code (neo-parser sample-code))
;(run-neo-parsed-code parsed-neo-code env)


;(define parsed-neo-code (neo-parser '(call (function (a) (call (function (r) a ) (a))) (5))))
(define parsed-neo-code (neo-parser '(call (function(x) (local-vars ((a 3) (b 7) (c 3)) (math + a b))) (5))))

(run-neo-parsed-code parsed-neo-code env)