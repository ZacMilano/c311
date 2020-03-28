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


(define (value-of-cps vo-to-eval vo-env-cps vo-k)
  (union-case vo-to-eval expr ; match 'to-eval' against union type 'expr'
    [(const const-expr) (apply-k vo-k const-expr)]
    [(mult x1 x2)
     (let* ([vo-k (kt_k-mult-n1 x2 vo-env-cps vo-k)]
            [vo-to-eval x1]
            [vo-env-cps vo-env-cps])
       (value-of-cps vo-to-eval vo-env-cps vo-k))]
    [(sub1 x)
     (let* ([vo-k (kt_k-sub1 vo-k)]
            [vo-to-eval x]
            [vo-env-cps vo-env-cps])
       (value-of-cps vo-to-eval vo-env-cps vo-k))]
    [(zero x)
     (let* ([vo-k (kt_k-zero? vo-k)]
            [vo-to-eval x]
            [vo-env-cps vo-env-cps])
       (value-of-cps vo-to-eval vo-env-cps vo-k))]
    [(if test conseq alt)
     (let* ([vo-k (kt_k-if conseq alt vo-env-cps vo-k)]
            [vo-to-eval test]
            [vo-env-cps vo-env-cps])
       (value-of-cps vo-to-eval vo-env-cps vo-k))]
    [(letcc body)
     (let* ([vo-k vo-k]
            [vo-to-eval body]
            [vo-env-cps (envr_extend-env vo-k vo-env-cps)])
       (value-of-cps vo-to-eval vo-env-cps vo-k))]
    [(throw k-exp v-exp)
     (let* ([vo-k (kt_k-throw v-exp vo-env-cps)]
            [vo-to-eval k-exp]
            [vo-env-cps vo-env-cps])
       (value-of-cps vo-to-eval vo-env-cps vo-k))]
    [(let e body)
     (let* ([vo-k (kt_k-let body vo-env-cps vo-k)]
            [vo-to-eval e]
            [vo-env-cps vo-env-cps])
         (value-of-cps vo-to-eval vo-env-cps vo-k))]
    [(var y)
     (apply-env vo-env-cps y vo-k)]
    [(lambda body)
     (apply-k vo-k (clos_closure body vo-env-cps))]
    [(app rator rand)
     (let* ([vo-k (kt_k-rator rand vo-env-cps vo-k)]
            [vo-to-eval rator]
            [vo-env-cps vo-env-cps])
       (value-of-cps vo-to-eval vo-env-cps vo-k))]))

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
     (let* ([vo-k (kt_k-mult-n2 v k^)]
            [vo-to-eval x2^]
            [vo-env-cps env-cps^])
       (value-of-cps vo-to-eval vo-env-cps vo-k))]
    [(k-sub1 k^) (apply-k k^ (sub1 v))]
    [(k-zero? k^) (apply-k k^ (zero? v))]
    [(k-if conseq^ alt^ env-cps^ k^)
     (if v
         (let* ([vo-k k^]
                [vo-to-eval conseq^]
                [vo-env-cps env-cps^])
           (value-of-cps vo-to-eval vo-env-cps vo-k))
         (let* ([vo-k k^]
                [vo-to-eval alt^]
                [vo-env-cps env-cps^])
           (value-of-cps vo-to-eval vo-env-cps vo-k)))]
    [(k-throw v-exp^ env-cps^)
     (let* ([vo-k v]
            [vo-to-eval v-exp^]
            [vo-env-cps env-cps^])
       (value-of-cps vo-to-eval vo-env-cps vo-k))]
    [(k-let body^ env-cps^ k^)
     (let* ([vo-k k^]
            [vo-to-eval body^]
            [vo-env-cps (envr_extend-env v env-cps^)])
       (value-of-cps vo-to-eval vo-env-cps vo-k))]
    [(k-operand c-cps^ k^) (apply-closure c-cps^ v k^)]
    [(k-rator rand^ env-cps^ k^)
     (let* ([vo-k (kt_k-operand v k^)]
            [vo-to-eval rand^]
            [vo-env-cps env-cps^])
       (value-of-cps vo-to-eval vo-env-cps vo-k))]
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
     (let* ([vo-k k^]
            [vo-to-eval body]
            [vo-env-cps (envr_extend-env a env-cps)])
       (value-of-cps vo-to-eval vo-env-cps vo-k))]))


; for debugging
(define (d exp)
  (let* ([vo-k (empty-k)]
         [vo-to-eval exp]
         [vo-env-cps (empty-env)])
    (value-of-cps vo-to-eval vo-env-cps vo-k)))

(define empty-env
  (lambda ()
    (envr_empty-env)))

(define empty-k
  (lambda ()
    (kt_k-init)))



(define main 
  (lambda ()
    (let* ([vo-k (empty-k)]          
           [vo-env-cps (empty-env)]
           [vo-to-eval
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
      (value-of-cps vo-to-eval vo-env-cps vo-k))))

; Invocation for quick testing
(main)
