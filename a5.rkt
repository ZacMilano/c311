#lang typed/racket

(define-type Exp
  (U Boolean
     Number
     (List 'zero? Exp)
     (List 'sub1 Exp)
     (List '* Exp Exp)
     (List 'if Exp Exp Exp)
     (List 'set! Symbol Exp)
     (List 'begin2 Exp Exp)
     (List 'random Exp)
     Symbol
     (List 'lambda (List Symbol) Exp)
     (List Exp Exp)))

(define-type Env-Cbv
  (-> Symbol (Boxof Val-Cbv)))

(define-type Closure-Cbv
  (-> Val-Cbv Val-Cbv))

(define-type Val-Cbv
  (U Boolean
     Number
     Void
     Closure-Cbv))

(: not-settable String)
(define not-settable "CANNOT FIND IT, YO")

(: apply-closure-cbv (-> Closure-Cbv Val-Cbv Val-Cbv))
(define (apply-closure-cbv rator rand)
  (rator rand))

(: apply-env-cbv (-> Env-Cbv Symbol Val-Cbv))
(define (apply-env-cbv env y)
  (unbox (env y)))

(: empty-env-cbv (-> Env-Cbv))
(define (empty-env-cbv) (lambda (y) (error "Unbound variable" y "in cbv")))

(: extend-env-cbv (-> Symbol Val-Cbv Env-Cbv Env-Cbv))
(define (extend-env-cbv x a env)
  (lambda (y)
    (if (eqv? y x)
        (box a)
        (box (apply-env-cbv env y)))))


(: val-of-cbv (-> Exp Env-Cbv Val-Cbv))
(define (val-of-cbv exp env)
  (match exp
    [`,b #:when (boolean? b) b]
    [`,n #:when (number? n)  n]
    [`(zero? ,n)
     (let ([res (val-of-cbv n env)])
       (if (not (number? res))
           (error "Type mismatch; given" res "in call to zero? in val-of-cbv")
           (zero? res)))]
    [`(sub1 ,n)
     (let ([res (val-of-cbv n env)])
       (if (not (number? res))
           (error "Type mismatch; given" res "in call to sub1 in val-of-cbv")
           (sub1 res)))]
    [`(* ,n1 ,n2)
     (let ([res1 (val-of-cbv n1 env)]
           [res2 (val-of-cbv n2 env)])
     (if (not (and (number? res1) (number? res2)))
         (error "Type mismatch; given" res1 res2 "in call to * in val-of-cbv")
         (* res1 res2)))]
    [`(if ,test ,conseq ,alt)
     (let ([test-res (val-of-cbv test env)])
       (if test-res
           (val-of-cbv conseq env)
           (val-of-cbv alt env)))]
    #;[`(set! ,x ,e)
     (let ([v (val-of-cbv e env)])
       (cond
         [(eqv? not-settable
                (with-handlers
                    ([exn:fail? (lambda (exn) not-settable)])
                  (apply-env-cbv env x)))
          (error "Cannot set! var" x "before its declaration")]
         [else (set-box! (apply-env-cbv env x) v)]))]
    [`(begin2 ,e1 ,e2) (begin (val-of-cbv e1 env) (val-of-cbv e2 env))]
    [`(random ,n)
     (let ([res (val-of-cbv n env)])
       (if (not (number? res))
           (error "Type mismatch; given" res "in call to random in val-of-cbv")
           (random (cast res Integer))))]
    [`,y #:when (symbol? y) (apply-env-cbv env y)]
    [`(lambda (,x) ,body) (make-closure-cbv x body env)]
    [`(,rator ,rand) #:when (symbol? rand)
                     (let ([clos (val-of-cbv rator env)]
                           [a (apply-env-cbv env rand)])
                       (if (or (boolean? clos)
                               (number? clos)
                               (symbol? clos)
                               (void? clos))
                           (error "Type mismatch; given" clos "as rator in val-of-cbv")
                           (apply-closure-cbv clos a)))]
    [`(,rator ,rand)
     (let ([clos (val-of-cbv rator env)]
           [a (val-of-cbv rand env)])
       (if (or (boolean? clos)
               (number? clos)
               (symbol? clos)
               (void? clos))
           (error "Type mismatch; given" clos "as rator in val-of-cbv")
       (apply-closure-cbv clos a)))]))


(: make-closure-cbv (-> Symbol Exp Env-Cbv Closure-Cbv))
(define (make-closure-cbv x body env)
  (lambda (a) (val-of-cbv body (extend-env-cbv x a env))))









(define-type Env-Cbr
  (-> Symbol (Boxof Val-Cbr)))

(define-type Val-Cbr
  (U Boolean
     Number
     Void
     Closure-Cbr))

(define-type Closure-Cbr
  (→ (Boxof Val-Cbr) Val-Cbr))

(: apply-env-cbr (-> Env-Cbr Symbol Val-Cbr))
(define (apply-env-cbr env y)
  (unbox (env y)))

(: make-closure-cbr (-> Symbol Exp Env-Cbr Closure-Cbr))
(define (make-closure-cbr x body env)
  (lambda (a) (val-of-cbr body (extend-env-cbr x a env))))

(: apply-closure-cbr (-> Closure-Cbr (Boxof Val-Cbr) Val-Cbr))
(define (apply-closure-cbr rator rand)
  (rator rand))


(: empty-env-cbr (-> Env-Cbr))
(define (empty-env-cbr) (lambda (y) (error "Unbound variable" y "in cbr")))

(: extend-env-cbr (-> Symbol (Boxof Val-Cbr) Env-Cbr Env-Cbr))
(define (extend-env-cbr x a env)
  (lambda (y)
    (if (eqv? y x)
        a
        (box (apply-env-cbr env y)))))

(: val-of-cbr (-> Exp Env-Cbr Val-Cbr))
(define (val-of-cbr exp env)
  (match exp
    [`,b #:when (boolean? b) b]
    [`,n #:when (number? n)  n]
    [`(zero? ,n)
     (let ([res (val-of-cbr n env)])
       (if (not (number? res))
           (error "Type mismatch; given" res "in call to zero? in val-of-cbr")
           (zero? res)))]
    [`(sub1 ,n)
     (let ([res (val-of-cbr n env)])
       (if (not (number? res))
           (error "Type mismatch; given" res "in call to sub1 in val-of-cbr")
           (sub1 res)))]
    [`(* ,n1 ,n2)
     (let ([res1 (val-of-cbr n1 env)]
           [res2 (val-of-cbr n2 env)])
     (if (not (and (number? res1) (number? res2)))
         (error "Type mismatch; given" res1 res2 "in call to * in val-of-cbr")
         (* res1 res2)))]
    [`(if ,test ,conseq ,alt)
     (let ([test-res (val-of-cbr test env)])
       (if test-res
           (val-of-cbr conseq env)
           (val-of-cbr alt env)))]
    #;[`(set! ,x ,e)
     (let ([v (val-of-cbr e env)])
       (cond
         [(eqv? not-settable
                (with-handlers
                    ([exn:fail? (lambda (exn) not-settable)])
                  (apply-env-cbv env x)))
          (error "Cannot set! var" x "before its declaration")]
         [else (set-box! x v)]))]
    [`(begin2 ,e1 ,e2) (begin (val-of-cbr e1 env) (val-of-cbr e2 env))]
    [`(random ,n)
     (let ([res (val-of-cbr n env)])
       (if (not (number? res))
           (error "Type mismatch; given" res "in call to random in val-of-cbr")
           (random (cast res Integer))))]
    [`,y #:when (symbol? y) (apply-env-cbr env y)]
    [`(lambda (,x) ,body) (make-closure-cbr x body env)]
    [`(,rator ,rand) #:when (symbol? rand)
                     (let ([clos (val-of-cbr rator env)]
                           [a (box (apply-env-cbr env rand))])
                       (if (or (boolean? clos)
                               (number? clos)
                               (symbol? clos)
                               (void? clos))
                           (error "Type mismatch; given" clos "as rator in val-of-cbr")
                           (apply-closure-cbr clos a)))]
    [`(,rator ,rand)
     (let ([clos (val-of-cbr rator env)]
           [a (val-of-cbr rand env)])
       (if (or (boolean? clos)
               (number? clos)
               (symbol? clos)
               (void? clos))
           (error "Type mismatch; given" clos "as rator in val-of-cbr")
       (apply-closure-cbr clos (box a))))]))










(define-type Env-Cbname
  (-> Symbol (-> Val-Cbname)))

(define-type Val-Cbname
  (U Boolean
     Number
     Void
     Closure-Cbname))

(define-type Closure-Cbname
  (→ (-> Val-Cbname) Val-Cbname))

(: apply-env-cbname (-> Env-Cbname Symbol Val-Cbname))
(define (apply-env-cbname env y)
  ((env y)))

(: make-closure-cbname (-> Symbol Exp Env-Cbname Closure-Cbname))
(define (make-closure-cbname x body env)
  (lambda (a) (val-of-cbname body (extend-env-cbname x a env))))

(: apply-closure-cbname (-> Closure-Cbname (-> Val-Cbname) Val-Cbname))
(define (apply-closure-cbname rator rand)
  (rator rand))

(: extend-env-cbname (-> Symbol (-> Val-Cbname) Env-Cbname Env-Cbname))
(define (extend-env-cbname x a env)
  (lambda (y)
    (if (eqv? y x)
        a
        (lambda () (apply-env-cbname env y)))))


(: empty-env-cbname (-> Env-Cbname))
(define (empty-env-cbname) (lambda (y) (error "Unbound variable" y "in cbname")))

(: val-of-cbname (-> Exp Env-Cbname Val-Cbname))
(define (val-of-cbname exp env)
  (match exp
    [`,b #:when (boolean? b) b]
    [`,n #:when (number? n)  n]
    [`(zero? ,n)
     (let ([res (val-of-cbname n env)])
       (if (not (number? res))
           (error "Type mismatch; given" res "in call to zero? in val-of-cbname")
           (zero? res)))]
    [`(sub1 ,n)
     (let ([res (val-of-cbname n env)])
       (if (not (number? res))
           (error "Type mismatch; given" res "in call to sub1 in val-of-cbname")
           (sub1 res)))]
    [`(* ,n1 ,n2)
     (let ([res1 (val-of-cbname n1 env)]
           [res2 (val-of-cbname n2 env)])
     (if (not (and (number? res1) (number? res2)))
         (error "Type mismatch; given" res1 res2 "in call to * in val-of-cbname")
         (* res1 res2)))]
    [`(if ,test ,conseq ,alt)
     (let ([test-res (val-of-cbname test env)])
       (if test-res
           (val-of-cbname conseq env)
           (val-of-cbname alt env)))]
    [`(random ,n)
     (let ([res (val-of-cbname n env)])
       (if (not (number? res))
           (error "Type mismatch; given" res "in call to random in val-of-cbname")
           (random (cast res Integer))))]
    [`,y #:when (symbol? y) (apply-env-cbname env y)]
    [`(lambda (,x) ,body) (make-closure-cbname x body env)]
    [`(,rator ,rand)
     (let ([clos (val-of-cbname rator env)]
           [th (lambda () (val-of-cbname rand env))])
       (if (or (boolean? clos)
               (number? clos)
               (symbol? clos)
               (void? clos))
           (error "Type mismatch; given" clos "as rator in val-of-cbname")
       (apply-closure-cbname clos th)))]))








(define-type Env-Cbneed
  (-> Symbol (Boxof (-> Val-Cbneed))))

(define-type Val-Cbneed
  (U Boolean
     Number
     Void
     Closure-Cbneed))

(define-type Closure-Cbneed
  (→ (Boxof (-> Val-Cbneed)) Val-Cbneed))

(: apply-env-cbneed (-> Env-Cbneed Symbol Val-Cbneed))
(define (apply-env-cbneed env y)
  ((unbox (env y))))

(: make-closure-cbneed (-> Symbol Exp Env-Cbneed Closure-Cbneed))
(define (make-closure-cbneed x body env)
  (lambda (a) (val-of-cbneed body (extend-env-cbneed x a env))))

(: apply-closure-cbneed (-> Closure-Cbneed (Boxof (-> Val-Cbneed)) Val-Cbneed))
(define (apply-closure-cbneed rator rand)
  (rator rand))

(: extend-env-cbneed (-> Symbol (Boxof (-> Val-Cbneed)) Env-Cbneed Env-Cbneed))
(define (extend-env-cbneed x a env)
  (lambda (y)
    (if (eqv? y x)
        a
        (box (lambda () (apply-env-cbneed env y))))))


(: empty-env-cbneed (-> Env-Cbneed))
(define (empty-env-cbneed) (lambda (y) (error "Unbound variable" y "in cbneed")))

(: val-of-cbneed (-> Exp Env-Cbneed Val-Cbneed))
(define (val-of-cbneed exp env)
  (match exp
    [`,b #:when (boolean? b) b]
    [`,n #:when (number? n)  n]
    [`(zero? ,n)
     (let ([res (val-of-cbneed n env)])
       (if (not (number? res))
           (error "Type mismatch; given" res "in call to zero? in val-of-cbneed")
           (zero? res)))]
    [`(sub1 ,n)
     (let ([res (val-of-cbneed n env)])
       (if (not (number? res))
           (error "Type mismatch; given" res "in call to sub1 in val-of-cbneed")
           (sub1 res)))]
    [`(* ,n1 ,n2)
     (let ([res1 (val-of-cbneed n1 env)]
           [res2 (val-of-cbneed n2 env)])
     (if (not (and (number? res1) (number? res2)))
         (error "Type mismatch; given" res1 res2 "in call to * in val-of-cbneed")
         (* res1 res2)))]
    [`(if ,test ,conseq ,alt)
     (let ([test-res (val-of-cbneed test env)])
       (if test-res
           (val-of-cbneed conseq env)
           (val-of-cbneed alt env)))]
    [`(random ,n)
     (let ([res (val-of-cbneed n env)])
       (if (not (number? res))
           (error "Type mismatch; given" res "in call to random in val-of-cbneed")
           (random (cast res Integer))))]
    [`,y #:when (symbol? y) (apply-env-cbneed env y)]
    [`(lambda (,x) ,body) (make-closure-cbneed x body env)]
    [`(,rator ,rand) #:when (symbol? rand)
                     (let ([clos (val-of-cbneed rator env)]
                           [a (box (lambda () (apply-env-cbneed env rand)))])
                       (if (or (boolean? clos)
                               (number? clos)
                               (symbol? clos)
                               (void? clos))
                           (error "Type mismatch; given" clos "as rator in val-of-cbneed")
                           (apply-closure-cbneed clos a)))]
    [`(,rator ,rand)
     (let ([clos (val-of-cbneed rator env)]
           [b (box (lambda () (val-of-cbneed rand env)))])
       (if (or (boolean? clos)
               (number? clos)
               (symbol? clos)
               (void? clos))
           (error "Type mismatch; given" clos "as rator in val-of-cbneed")
       (apply-closure-cbneed clos b)))]))





#|

> (val-of-cbname
   '((lambda (z) 100)
     ((lambda (x) (x x)) (lambda (x) (x x))))
   (empty-env-cbname))
- : Val-Cbname
100
> (val-of-cbneed
   '((lambda (z) 100)
     ((lambda (x) (x x)) (lambda (x) (x x))))
   (empty-env-cbneed))
- : Val-Cbneed
100


|#
























































































