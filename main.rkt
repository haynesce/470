#lang racket

(require "utility.rkt")
(require "runner.rkt")
(require "parser.rkt")
(require "variable_env.rkt")

(define env '((a 1) (b 2) (c 5)))

(define sample-code '(call (function () (ask (bool != a b) (math - a b) (math + a b))) (a)))
(display (neo-parser sample-code))
(define parsed-neo-code (neo-parser sample-code))
(run-neo-parsed-code parsed-neo-code env)

(elementAt '(!= (var-exp a) (var-exp b)) 1)