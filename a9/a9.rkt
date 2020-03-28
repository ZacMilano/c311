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

(define-union kt
  (k-init)
  (k-mult-n2 n1^ k^)
  (k-mult-n1 x2^ env-cps^ k^)
  (k-sub1 k^)
  (k-zero? k^)
  (k-if conseq^ alt^ env-cps^ k^)
  (k-throw v-exp^ env-cps^)
  (k-let body^ env-cps^ k^)
  (k-operand c-cps^ k^)
  (k-rator rand^ env-cps^ k^))


(define (value-of-cps to-eval env-cps k)
  (union-case to-eval expr ; match 'to-eval' against union type 'expr'
    [(const const-expr) (apply-k k const-expr)]
    [(mult x1 x2)
     (let* ([k (kt_k-mult-n1 x2 env-cps k)]
            [to-eval x1]
            [env-cps env-cps])
       (value-of-cps to-eval env-cps k))]
    [(sub1 x)
     (let* ([k (kt_k-sub1 k)]
            [to-eval x]
            [env-cps env-cps])
       (value-of-cps to-eval env-cps k))]
    [(zero x)
     (let* ([k (kt_k-zero? k)]
            [to-eval x]
            [env-cps env-cps])
       (value-of-cps to-eval env-cps k))]
    [(if test conseq alt)
     (let* ([k (kt_k-if conseq alt env-cps k)]
            [to-eval test]
            [env-cps env-cps])
       (value-of-cps to-eval env-cps k))]
    [(letcc body)
     (let* ([k k]
            [to-eval body]
            [env-cps (envr_extend-env k env-cps)])
       (value-of-cps to-eval env-cps k))]
    [(throw k-exp v-exp)
     (let* ([k (kt_k-throw v-exp env-cps)]
            [to-eval k-exp]
            [env-cps env-cps])
       (value-of-cps to-eval env-cps k))]
    [(let e body)
     (let* ([k (kt_k-let body env-cps k)]
            [to-eval e]
            [env-cps env-cps])
         (value-of-cps to-eval env-cps k))]
    [(var y)
     (apply-env env-cps y k)]
    [(lambda body)
     (apply-k k (clos_closure body env-cps))]
    [(app rator rand)
     (let* ([k (kt_k-rator rand env-cps k)]
            [to-eval rator]
            [env-cps env-cps])
       (value-of-cps to-eval env-cps k))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#|
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
|#
(define (apply-k k v)
  (union-case k kt
    [(k-mult-n2 n1^ k^) (apply-k k^ (* n1^ v))]
    [(k-mult-n1 x2^ env-cps^ k^)
     (let* ([k (kt_k-mult-n2 v k^)]
            [to-eval x2^]
            [env-cps env-cps^])
       (value-of-cps to-eval env-cps k))]
    [(k-sub1 k^) (apply-k k^ (sub1 v))]
    [(k-zero? k^) (apply-k k^ (zero? v))]
    [(k-if conseq^ alt^ env-cps^ k^)
     (if v
         (let* ([k k^]
                [to-eval conseq^]
                [env-cps env-cps^])
           (value-of-cps to-eval env-cps k))
         (let* ([k k^]
                [to-eval alt^]
                [env-cps env-cps^])
           (value-of-cps to-eval env-cps k)))]
    [(k-throw v-exp^ env-cps^)
     (let* ([k v]
            [to-eval v-exp^]
            [env-cps env-cps^])
       (value-of-cps to-eval env-cps k))]
    [(k-let body^ env-cps^ k^)
     (let* ([k k^]
            [to-eval body^]
            [env-cps (envr_extend-env v env-cps^)])
       (value-of-cps to-eval env-cps k))]
    [(k-operand c-cps^ k^) (apply-closure c-cps^ v k^)]
    [(k-rator rand^ env-cps^ k^)
     (let* ([k (kt_k-operand v k^)]
            [to-eval rand^]
            [env-cps env-cps^])
       (value-of-cps to-eval env-cps k))]
    [(k-init) v]))

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
    [(closure body env-cps)
     (let* ([k k^]
            [to-eval body]
            [env-cps (envr_extend-env a env-cps)])
       (value-of-cps to-eval env-cps k))]))


; for debugging
(define (d exp)
  (let* ([k (empty-k)]
         [to-eval exp]
         [env-cps (empty-env)])
    (value-of-cps to-eval env-cps k)))

(define empty-env
  (lambda ()
    (envr_empty-env)))

(define empty-k
  (lambda ()
    (kt_k-init)))



(define main 
  (lambda ()
    (let* ([k (empty-k)]          
           [env-cps (empty-env)]
           [to-eval
            (expr_let 
             (expr_lambda
              (expr_lambda 
               (expr_if
                (expr_zero (expr_var 0))
                (expr_const 1)
                (expr_mult (expr_var 0) (expr_app (expr_app (expr_var 1) (expr_var 1))
                                                  (expr_sub1 (expr_var 0)))))))
             (expr_mult
              (expr_letcc
               (expr_app
                (expr_app (expr_var 1) (expr_var 1))
                (expr_throw (expr_var 0) (expr_app (expr_app (expr_var 1) (expr_var 1)) (expr_const 4)))))
              (expr_const 5)))])
      (value-of-cps to-eval env-cps k))))

; Invocation for quick testing
(main)
