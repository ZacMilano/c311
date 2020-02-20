#lang typed/racket

(define-type Exp
  (U Boolean
     Number
     (List 'zero? Exp)
     (List 'sub1 Exp)
     (List '* Exp Exp)
     (List 'if Exp Exp Exp)
     (List 'begin2 Exp Exp)
     (List 'random Exp)
     Symbol
     (List 'lambda (List Symbol) Exp)
     (List Exp Exp)))

(define-type Env
  (-> Symbol Val))

(define-type Closure
  (-> Val Val))

(define-type Val
  (U Boolean
     Number
     Closure))

(: apply-closure (-> Closure Val Val))
(define (apply-closure rator rand)
  (rator rand))

(: apply-env (-> Env Symbol Val))
(define (apply-env env y)
  (env y))


(: value-of-cbv (-> Exp Env Val))
(define (value-of-cbv exp env)
  (match exp
    [`,b #:when (boolean? b) b]
    [`,n #:when (number? n)  n]
    [`(zero? ,n)
     (let ([res (value-of-cbv n env)])
       (if (not (number? res))
           (error "Type mismatch; given" res "in call to zero? in value-of-cbv")
           (zero? res)))]
    [`(sub1 ,n)
     (let ([res (value-of-cbv n env)])
       (if (not (number? res))
           (error "Type mismatch; given" res "in call to sub1 in value-of-cbv")
           (sub1 res)))]
    [`(* ,n1 ,n2)
     (let ([res1 (value-of-cbv n1 env)]
           [res2 (value-of-cbv n2 env)])
     (if (not (and (number? res1) (number? res2)))
         (error "Type mismatch; given" res1 res2 "in call to * in value-of-cbv")
         (* res1 res2)))]
    [`(if ,test ,conseq ,alt)
     (let ([test-res (value-of-cbv test env)])
       (if test-res
           (value-of-cbv conseq env)
           (value-of-cbv alt env)))]
    [`(begin2 ,e1 ,e2) (begin (value-of-cbv e1 env) (value-of-cbv e2 env))]
    [`(random ,n)
     (let ([res (value-of-cbv n env)])
       (if (not (number? res))
           (error "Type mismatch; given" res "in call to random in value-of-cbv")
           (random res)))]
    [`,y #:when (symbol? y) (apply-env env y)]
    [`(lambda (,x) ,body) (make-closure-cbv x body env)]
    [`(,rator ,rand) (apply-closure (value-of-cbv rator env)
                                    (value-of-cbv rand env))]))


(: make-closure-cbv (-> Symbol Exp Env Closure))
(define (make-closure-cbv x body env)
  (lambda (a) a))


































