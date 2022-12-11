;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname neo10.5) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(define env '((a 1) (b 2) (c 5)))
;resolve a
(define resolve
  (lambda (environment varname)
    (cond
      ((null? environment) #false)
      ((equal? (caar environment) varname) (cadar environment))
      (else (resolve (cdr environment) varname))
      )
    )
  )

;(resolve env 'a)

(define neo-parser
  (lambda (neo-code)
    (cond
      ((null? neo-code) '())
      ((number? neo-code) (list 'num-exp neo-code))
      ((symbol? neo-code) (list 'var-exp neo-code))
      ;(bool op num1 num2) > (bool-exp op (neo-exp) (neo-exp))
       ((equal? (car neo-code) 'bool)
        (if (equal? (length neo-code) 3)
            (list 'bool-exp (cadr neo-code) (neo-parser (caddr neo-code)) '())
        (cons 'bool-exp (cons (cadr neo-code) (map neo-parser (cddr neo-code))))))
      ;(math op num1 num2) > (math-exp op (neo-exp) (neo-exp))
      ((equal? (car neo-code) 'math)
       (list 'math-exp (cadr neo-code)
             (neo-parser (caddr neo-code))
             (neo-parser (cadddr neo-code))))
      ;(ask (bool op num1 num2) (neo-exp1) (neo-exp2)) > (ask-exp (bool-exp ...) (parsed-neo-exp1) (parsed-neo-exp2))
      ((equal? (car neo-code) 'ask)
       (cons 'ask-exp
             (map neo-parser (cdr neo-code))))
      ;(function (x y z,...) x)
      ((equal? (car neo-code) 'function)
       (list 'func-exp
             (list 'params (cadr neo-code))
             (list 'body-exp (neo-parser (caddr neo-code)))))
      ;(call (function (x y z) (math + (math + x y) z)) (1 2 3)) ->
      ;(app-exp (func-exp (params (identifier1, identifier2, identifer3 ...)) (body-exp)) ((neo-exp1 neo-exp2 neo-exp3 ...))
      ((equal? (car neo-code) 'call)
       (list 'app-exp
             (neo-parser (cadr neo-code))
             (neo-parser (caddr neo-code))))
      (else (map neo-parser neo-code)) ;((neo-parser 1) (neo-parser 'a) (neo-parser (math + 1 2)))
      )
    )
  )

;env = ((a 1) (b 2) (c 5)
(define extend-env
  (lambda (list-of-varname list-of-value env) ;((x y z) (1 2 3) env)
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

;(app-exp (func-exp (params x) (body-exp (var-exp x))) (var-exp a))
(define run-neo-parsed-code
  (lambda (parsed-code env)
    (cond
      ((null? parsed-code) '())
      ((equal? (car parsed-code) 'num-exp)
       (cadr parsed-code));(num-exp 22)
      ((equal? (car parsed-code) 'var-exp)
       (resolve env (cadr parsed-code)))
      ;(bool-exp op (neo-exp) (neo-exp))
      ((equal? (car parsed-code) 'bool-exp)
       (run-bool-exp (cadr parsed-code)
                     (run-neo-parsed-code (caddr parsed-code) env)
                     (run-neo-parsed-code (cadddr parsed-code) env)))
      ;(math-exp op (neo-exp) (neo-exp))
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
      (else (run-neo-parsed-code
             (cadr parsed-code) ;function expression
             (extend-env
              (cadr (cadr (cadr parsed-code)))
              (map (lambda (exp) (run-neo-parsed-code exp env)) (caddr parsed-code));list of values ((num-exp 1) (var-exp a) (math-exp + (num-exp 2) (num-exp 3)))
              env);environment scope update
             )
            )
      )
    ) 
  )

;tool to understand bool-exp
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

(define sample-code '(call (function () (ask (bool > a b) (math - a b) (math + a b))) (a)))
(define parsed-neo-code (neo-parser sample-code))
; (run-neo-parsed-code parsed-neo-code env)

; (define sample-code '(call (function(x) (math + 1 1)) 4))
; (define sample-neo-code '(call (function (x) x) a))
; (display (neo-parser sample-neo-code))