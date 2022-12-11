#lang racket

(require "utility.rkt")
;resolve a value from variable environment
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

(define run-neo-parsed-code
  (lambda (parsed-code env)
    (cond
      ((null? parsed-code) '())
      ((equal? (car parsed-code) 'num-exp)
       (cadr parsed-code));(num-exp 22)
      ((equal? (car parsed-code) 'var-exp)
       (resolve env (cadr parsed-code)))
      ;(bool-exp op (neo-exp) (neo-exp))
      ((equal? (car parsed-code) 'bool-exp) (run-bool-parsed-code (cdr parsed-code) env))
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

(define run-math-exp
  (lambda (op num1 num2)
    (cond
      ((equal? op '+) (+ num1 num2)) ;+ 1 1)
      ((equal? op '-) (- num1 num2))  ;- 1 1
      ((equal? op '*) (* num1 num2)) ;* 1 1
      ((equal? op '/) (/ num1 num2))  ;/ 1 1 float division
      ((equal? op '//) (quotient num1 num2))  ;// 1 1 interger division
      ((equal? op '%) (modulo num1 num2))  ;% 1 1 modulo
      (else #false)
      )
    )
  )

(provide (all-defined-out))