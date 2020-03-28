#lang racket
(require "parenthec.rkt")


(define-union expr
  (const cexp)
  (var n)
  (if test conseq alt)
  (mult nexp1 nexp2)
  (sub1 nexp)
  (zero nexp)
  (letcc body)
  (throw kexp vexp)
  (let exp body)              
  (lambda body)
  (app rator rand))

(define-union clos
  (closure body env-cps))

(define-union envr
  (empty-env)
  (extend-env value^ env-cps^))

(define (value-of-cps to-eval env-cps k)
  (union-case to-eval expr ; match 'to-eval' against union type 'expr'
    [(const const-expr) (apply-k k const-expr)]
    [(mult x1 x2)
     (value-of-cps
      x1 env-cps
      (make-k-mult-n1 x2 env-cps k))]
    [(sub1 x)
     (value-of-cps
      x env-cps
      (make-k-sub1 k))]
    [(zero x)
     (value-of-cps
      x env-cps
      (make-k-zero? k))]
    [(if test conseq alt)
     (value-of-cps
      test env-cps
      (make-k-if conseq alt env-cps k))]
    [(letcc body)
     (value-of-cps body (envr_extend-env k env-cps) k)]
    [(throw k-exp v-exp)
     (value-of-cps k-exp env-cps
                   (make-k-throw v-exp env-cps))]
    [(let e body)
     (value-of-cps
      e env-cps
      (make-k-let body env-cps k))]
    [(var y)
     (apply-env env-cps y k)]
    [(lambda body)
     (apply-k k (clos_closure body env-cps))]
    [(app rator rand)
     (value-of-cps rator env-cps (make-k-rator rand env-cps k))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-k-mult-n2 n1^ k^)
  `(k-mult-n2 ,n1^ ,k^))

(define (make-k-mult-n1 x2^ env-cps^ k^)
  `(k-mult-n1 ,x2^ ,env-cps^ ,k^))

(define (make-k-sub1 k^)
  `(k-sub1 ,k^))

(define (make-k-zero? k^)
  `(k-zero? ,k^))

(define (make-k-if conseq^ alt^ env-cps^ k^)
  `(k-if ,conseq^ ,alt^ ,env-cps^ ,k^))

(define (make-k-throw v-exp^ env-cps^)
  `(k-throw ,v-exp^ ,env-cps^))

(define (make-k-let body^ env-cps^ k^)
  `(k-let ,body^ ,env-cps^ ,k^))

(define (make-k-operand c-cps^ k^)
  `(k-operand ,c-cps^ ,k^))

(define (make-k-rator rand^ env-cps^ k^)
  `(k-rator ,rand^ ,env-cps^ ,k^))

(define (apply-k k v)
  (match k
    [`(k-mult-n2 ,n1^ ,k^) (apply-k k^ (* n1^ v))]
    [`(k-mult-n1 ,x2^ ,env-cps^ ,k^)
     (value-of-cps x2^ env-cps^ (make-k-mult-n2 v k^))]
    [`(k-sub1 ,k^) (apply-k k^ (sub1 v))]
    [`(k-zero? ,k^) (apply-k k^ (zero? v))]
    [`(k-if ,conseq^ ,alt^ ,env-cps^ ,k^)
     (if v
         (value-of-cps conseq^ env-cps^ k^)
         (value-of-cps alt^ env-cps^ k^))]
    [`(k-throw ,v-exp^ ,env-cps^) (value-of-cps v-exp^ env-cps^ v)]
    [`(k-let ,body^ ,env-cps^ ,k^) (value-of-cps body^ (envr_extend-env v env-cps^) k^)]
    [`(k-operand ,c-cps^ ,k^) (apply-closure c-cps^ v k^)]
    [`(k-rator ,rand^ ,env-cps^ ,k^)
     (value-of-cps rand^ env-cps^ (make-k-operand v k^))]
    [`(k-init) v]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(trace value-of-cps)

(define (apply-env env y k^)
  (union-case env envr
    [(extend-env value^ env-cps^)
     (if (zero? y)
         (apply-k k^ value^)
         (apply-env env-cps^ (sub1 y) k^))]
    [(empty-env) (error 'value-of-cps "unbound identifier")]))

(define (apply-closure c-cps a k^)
  (union-case c-cps clos
    [(closure body env-cps) (value-of-cps body (envr_extend-env a env-cps) k^)]))


; for debugging
(define (d exp)
  (value-of-cps exp (empty-env) (empty-k)))

(define empty-env
  (lambda ()
    (envr_empty-env)))

(define empty-k
  (lambda ()
    '(k-init)))



(define main 
  (lambda ()
    (value-of-cps 
     (expr_let 
      (expr_lambda
       (expr_lambda 
        (expr_if
         (expr_zero (expr_var 0))
         (expr_const 1)
         (expr_mult (expr_var 0) (expr_app (expr_app (expr_var 1) (expr_var 1)) (expr_sub1 (expr_var 0)))))))
      (expr_mult
       (expr_letcc
        (expr_app
         (expr_app (expr_var 1) (expr_var 1))
         (expr_throw (expr_var 0) (expr_app (expr_app (expr_var 1) (expr_var 1)) (expr_const 4)))))
       (expr_const 5)))
     (empty-env)
     (empty-k))))
