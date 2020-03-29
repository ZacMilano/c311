#lang racket
(require "parenthec.rkt")

(define-program-counter pc)

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
  (k-init jumpout)
  (k-mult-n2 n1^ k^)
  (k-mult-n1 x2^ env-cps^ k^)
  (k-sub1 k^)
  (k-zero? k^)
  (k-if conseq^ alt^ env-cps^ k^)
  (k-throw v-exp^ env-cps^)
  (k-let body^ env-cps^ k^)
  (k-operand c-cps^ k^)
  (k-rator rand^ env-cps^ k^))

; Registers
(define-registers
  vo-to-eval
  vo-env-cps
  vo-k
  ak-k
  ak-v
  ae-env
  ae-y
  ae-k^
  ac-c-cps
  ac-a
  ac-k^)

(define-label value-of-cps
  (union-case vo-to-eval expr ; match 'to-eval' against union type 'expr'
    [(const const-expr)
     (begin [set! ak-k vo-k]
            [set! ak-v const-expr]
            (set! pc apply-k))]
    [(mult x1 x2)
     (begin [set! vo-k (kt_k-mult-n1 x2 vo-env-cps vo-k)]
            [set! vo-to-eval x1]
            [set! vo-env-cps vo-env-cps]
            (set! pc value-of-cps))]
    [(sub1 x)
     (begin [set! vo-k (kt_k-sub1 vo-k)]
            [set! vo-to-eval x]
            [set! vo-env-cps vo-env-cps]
            (set! pc value-of-cps))]
    [(zero x)
     (begin [set! vo-k (kt_k-zero? vo-k)]
            [set! vo-to-eval x]
            [set! vo-env-cps vo-env-cps]
            (set! pc value-of-cps))]
    [(if test conseq alt)
     (begin [set! vo-k (kt_k-if conseq alt vo-env-cps vo-k)]
            [set! vo-to-eval test]
            [set! vo-env-cps vo-env-cps]
            (set! pc value-of-cps))]
    [(letcc body)
     (begin [set! vo-k vo-k]
            [set! vo-to-eval body]
            [set! vo-env-cps (envr_extend-env vo-k vo-env-cps)]
            (set! pc value-of-cps))]
    [(throw k-exp v-exp)
     (begin [set! vo-k (kt_k-throw v-exp vo-env-cps)]
            [set! vo-to-eval k-exp]
            [set! vo-env-cps vo-env-cps]
            (set! pc value-of-cps))]
    [(let e body)
     (begin [set! vo-k (kt_k-let body vo-env-cps vo-k)]
            [set! vo-to-eval e]
            [set! vo-env-cps vo-env-cps]
            (set! pc value-of-cps))]
    [(var y)
     (begin [set! ae-k^ vo-k]
            [set! ae-env vo-env-cps]
            [set! ae-y y]
            (set! pc apply-env))]
    [(lambda body)
     (begin [set! ak-k vo-k]
            [set! ak-v (clos_closure body vo-env-cps)]
            (set! pc apply-k))]
    [(app rator rand)
     (begin [set! vo-k (kt_k-rator rand vo-env-cps vo-k)]
            [set! vo-to-eval rator]
            [set! vo-env-cps vo-env-cps]
            (set! pc value-of-cps))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-label apply-k
  (union-case ak-k kt
    [(k-init jumpout) (dismount-trampoline jumpout)]
    [(k-mult-n2 n1^ k^)
     (begin [set! ak-k k^]
            [set! ak-v (* n1^ ak-v)]
            (set! pc apply-k))]
    [(k-mult-n1 x2^ env-cps^ k^)
     (begin [set! vo-k (kt_k-mult-n2 ak-v k^)]
            [set! vo-to-eval x2^]
            [set! vo-env-cps env-cps^]
            (set! pc value-of-cps))]
    [(k-sub1 k^)
     (begin [set! ak-k k^]
            [set! ak-v (sub1 ak-v)]
            (set! pc apply-k))]
    [(k-zero? k^)
     (begin [set! ak-k k^]
            [set! ak-v (zero? ak-v)]
            (set! pc apply-k))]
    [(k-if conseq^ alt^ env-cps^ k^)
     (if ak-v
         (begin [set! vo-k k^]
                [set! vo-to-eval conseq^]
                [set! vo-env-cps env-cps^]
                (set! pc value-of-cps))
         (begin [set! vo-k k^]
                [set! vo-to-eval alt^]
                [set! vo-env-cps env-cps^]
                (set! pc value-of-cps)))]
    [(k-throw v-exp^ env-cps^)
     (begin [set! vo-k ak-v]
            [set! vo-to-eval v-exp^]
            [set! vo-env-cps env-cps^]
            (set! pc value-of-cps))]
    [(k-let body^ env-cps^ k^)
     (begin [set! vo-k k^]
            [set! vo-to-eval body^]
            [set! vo-env-cps (envr_extend-env ak-v env-cps^)]
            (set! pc value-of-cps))]
    [(k-operand c-cps^ k^)
     (begin [set! ac-k^ k^]
            [set! ac-c-cps c-cps^]
            [set! ac-a ak-v]
            (set! pc apply-closure))]
    [(k-rator rand^ env-cps^ k^)
     (begin [set! vo-k (kt_k-operand ak-v k^)]
            [set! vo-to-eval rand^]
            [set! vo-env-cps env-cps^]
            (set! pc value-of-cps))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(trace value-of-cps)

(define-label apply-env
  (union-case ae-env envr
    [(extend-env value^ env-cps^)
     (if (zero? ae-y)
         (begin [set! ak-k ae-k^]
                [set! ak-v value^]
                (set! pc apply-k))
         (begin [set! ae-k^ ae-k^]
                [set! ae-env env-cps^]
                [set! ae-y (sub1 ae-y)]
                (set! pc apply-env)))]
    [(empty-env) (error 'value-of-cps "unbound identifier")]))

(define-label apply-closure
  (union-case ac-c-cps clos
    [(closure body env-cps)
     (begin [set! vo-k ac-k^]
            [set! vo-to-eval body]
            [set! vo-env-cps (envr_extend-env ac-a env-cps)]
            (set! pc value-of-cps))]))



(define-label empty-env
  (envr_empty-env))

#;
(define-label empty-k
  (kt_k-init))



(define-label main 
  (begin #;[set! vo-k (empty-k)]          
         [set! vo-env-cps (empty-env)]
         [set! vo-to-eval
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
                 (expr_const 5)))]
         (set! pc value-of-cps)
         (mount-trampoline kt_k-init vo-k pc)
         (printf "Fact 5: ~s\n" ak-v)))

; Invocation for quick testing
(main)
