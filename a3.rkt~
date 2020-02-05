#lang typed/racket
(require typed/rackunit)


(define-type Exp
  (U Symbol
     Number
     Boolean
     (List 'sub1 Exp)
     (List '* Exp Exp)
     (List 'if Exp Exp Exp)
     (List 'let (Listof LetDef) Exp)
     (List 'lambda (List Symbol) Exp)
     (List Exp Exp)))
(define-type Closure
  (→ Val Val))
(define-type Val
  (U Number
     Boolean
     Closure))
(define-type LetDef
  (List Symbol Exp))

(define-type Env
  (-> Symbol Val))

(define-syntax test-runner
  (syntax-rules (>)
    [(_) "All tests passed!"]
    [(_ > test result more ...)
     (begin (check-equal? test 'result)
            (test-runner more ...))]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; value-of
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(: value-of (-> Exp Env Val))
(define (value-of exp env)
  (match exp
    [`,y #:when (symbol? y) (env y)]
    [`,n #:when (number? n) n]
    [`,b #:when (boolean? b) b]
    [`(zero? ,e)
     (let ([res (value-of e env)])
       (cond
         [(number? res) (zero? res)]
         [else (error "Type mismatch; given" res "in call to zero? in value-of")]))]
    [`(sub1 ,e)
     (let ([res (value-of e env)])
       (cond
         [(number? res) (sub1 res)]
         [else (error "Type mismatch; given" res "in call to sub1 in value-of")]))]
    [`(* ,e1 ,e2)
     (let ([r1 (value-of e1 env)]
           [r2 (value-of e2 env)])
       (cond
         [(and (number? r1) (number? r2)) (* r1 r2)]
         [else (error "Tried to multiply non-numbers" r1 "and" r2 "in value-of")]))]
    [`(if ,predicate ,consequent ,alternative)
     (let ([c (value-of predicate env)])
       (cond
         [(not (boolean? c)) (error "Given non-boolean" c " as 'if' condition in value-of")]
         [c (value-of consequent env)]
         [(not c) (value-of alternative env)]))]
    [`(let ,lets ,body)
     (cond
       [(null? lets) (value-of body env)]
       [else
        (let ([sym (car (car lets))]
              [defined-as (value-of (car (cdr (car lets))) env)])
          (value-of (list 'let (cdr lets) body)
                       (lambda (y)
                         (if (eqv? y sym)
                             defined-as
                             (env y)))))])]
    [`(lambda (,x) ,body)
     (lambda ([a : Val]) (value-of body (lambda (y)
                                             (if (eqv? y x)
                                                 a
                                                 (env y)))))]
    [`(,rator ,rand)
     (let ([clos (value-of rator env)]
           [a (value-of rand env)])
       (cond
         [(procedure? clos) (clos a)]
         [else (error "Your rator" clos "is not a function in value-of")]))]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; value-of Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(test-runner
 > (value-of
   '((lambda (x) (if (zero? x)
                     #t
                     #f))
     0)
   (lambda (y) (error 'value-of "unbound variable ~s" y)))
#t                  
> (value-of 
   '((lambda (x) (if (zero? x) 
                     12 
                     47)) 
     0) 
   (lambda (y) (error 'value-of "unbound variable ~s" y)))
12    
> (value-of
   '(let ([y (* 3 4)])
      ((lambda (x) (* x y)) (sub1 6)))
   (lambda (y) (error 'value-of "unbound variable ~s" y)))
60
> (value-of
   '(let ([x (* 2 3)])
      (let ([y (sub1 x)])
        (* x y)))
   (lambda (y) (error 'value-of "unbound variable ~s" y)))
30
> (value-of
   '(let ([x (* 2 3)])
      (let ([x (sub1 x)])
        (* x x)))
   (lambda (y) (error 'value-of "unbound variable ~s" y)))
25 
> (value-of 
   '(let ((! (lambda (x) (* x x))))
      (let ((! (lambda (n)
                 (if (zero? n) 1 (* n (! (sub1 n)))))))
        (! 5)))
   (lambda (y) (error 'value-of "unbound variable ~s" y)))
80
> (value-of
   '(((lambda (f)
        (lambda (n) (if (zero? n) 1 (* n ((f f) (sub1 n))))))
      (lambda (f)
        (lambda (n) (if (zero? n) 1 (* n ((f f) (sub1 n)))))))
     5)
   (lambda (y) (error 'value-of "unbound variable ~s" y)))
120)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; value-of
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-type FEnv
  (→ Symbol Val))

(: empty-env-fn (-> FEnv))
(define (empty-env-fn)
  (lambda (y) (error "Unbound variable" y)))

(: extend-env-fn (-> Symbol Val FEnv FEnv))
(define (extend-env-fn x a env)
  (lambda (y)
    (if (eqv? y x)
        a
        (apply-env-fn env y))))

(: apply-env-fn (-> FEnv Symbol Val))
(define (apply-env-fn env y)
  (env y))

(: value-of-fn (→ Exp FEnv Val))
(define (value-of-fn exp env)
  (match exp
    [`,y #:when (symbol? y) (apply-env-fn env y)]
    [`,n #:when (number? n) n]
    [`,b #:when (boolean? b) b]
    [`(zero? ,e)
     (let ([res (value-of-fn e env)])
       (cond
         [(number? res) (zero? res)]
         [else (error "Type mismatch; given" res "in call to zero? in value-of-fn")]))]
    [`(sub1 ,e)
     (let ([res (value-of-fn e env)])
       (cond
         [(number? res) (sub1 res)]
         [else (error "Type mismatch; given" res "in call to sub1 in value-of-fn")]))]
    [`(* ,e1 ,e2)
     (let ([r1 (value-of-fn e1 env)]
           [r2 (value-of-fn e2 env)])
       (cond
         [(and (number? r1) (number? r2)) (* r1 r2)]
         [else (error "Tried to multiply non-numbers" r1 "and" r2 "in value-of-fn")]))]
    [`(if ,predicate ,consequent ,alternative)
     (let ([c (value-of-fn predicate env)])
       (cond
         [(not (boolean? c)) (error "Given non-boolean" c " as 'if' condition in value-of-fn")]
         [c (value-of-fn consequent env)]
         [(not c) (value-of-fn alternative env)]))]
    [`(let ,lets ,body)
     (cond
       [(null? lets) (value-of-fn body env)]
       [else
        (let ([sym (car (car lets))]
              [defined-as (value-of-fn (car (cdr (car lets))) env)])
          (value-of-fn (list 'let (cdr lets) body) (extend-env-fn sym defined-as env)))])]
    [`(lambda (,x) ,body)
     (lambda ([a : Val]) (value-of-fn body (extend-env-fn x a env)))]
    [`(,rator ,rand)
     (let ([clos (value-of-fn rator env)]
           [a (value-of-fn rand env)])
       (cond
         [(procedure? clos) (clos a)]
         [else (error "Your rator" clos "is not a function in value-of-fn")]))]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; value-of-fn Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test-runner
 > (value-of-fn
    '((lambda (x) (if (zero? x)
                      #t
                      #f))
      0)
    (empty-env-fn))
 #t                  
 > (value-of-fn 
    '((lambda (x) (if (zero? x) 
                      12 
                      47)) 
      0) 
    (empty-env-fn))
 12    
 > (value-of-fn
    '(let ([y (* 3 4)])
       ((lambda (x) (* x y)) (sub1 6)))
    (empty-env-fn))
 60
 > (value-of-fn
    '(let ([x (* 2 3)])
       (let ([y (sub1 x)])
         (* x y)))
    (empty-env-fn))
 30
 > (value-of-fn
    '(let ([x (* 2 3)])
       (let ([x (sub1 x)])
         (* x x)))
    (empty-env-fn))
 25
 > (value-of-fn
    '(((lambda (f)
         (lambda (n) (if (zero? n) 1 (* n ((f f) (sub1 n))))))
       (lambda (f)
         (lambda (n) (if (zero? n) 1 (* n ((f f) (sub1 n)))))))
      5)
    (empty-env-fn))
 120)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; value-of-ds
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-type DSEnv
  (Listof (Pairof Symbol Val)))

(: empty-env-ds (-> DSEnv))
(define (empty-env-ds) '())

(: extend-env-ds (-> Symbol Val DSEnv DSEnv))
(define (extend-env-ds x a env)
  ...)

(: apply-env-ds (-> DSEnv Symbol Val))
(define (apply-env-ds env y)
  ...)

(: value-of-ds (-> Exp DSEnv Val))
(define (value-of-ds exp env)
  (match exp
    [`,y #:when (symbol? y) (apply-env-ds env y)]
    [`,n #:when (number? n) n]
    [`,b #:when (boolean? b) b]
    [`(zero? ,e)
     (let ([res (value-of-ds e env)])
       (cond
         [(number? res) (zero? res)]
         [else (error "Type mismatch; given" res "in call to zero? in value-of-ds")]))]
    [`(sub1 ,e)
     (let ([res (value-of-ds e env)])
       (cond
         [(number? res) (sub1 res)]
         [else (error "Type mismatch; given" res "in call to sub1 in value-of-ds")]))]
    [`(* ,e1 ,e2)
     (let ([r1 (value-of-ds e1 env)]
           [r2 (value-of-ds e2 env)])
       (cond
         [(and (number? r1) (number? r2)) (* r1 r2)]
         [else (error "Tried to multiply non-numbers" r1 "and" r2 "in value-of-ds")]))]
    [`(if ,predicate ,consequent ,alternative)
     (let ([c (value-of-ds predicate env)])
       (cond
         [(not (boolean? c)) (error "Given non-boolean" c " as 'if' condition in value-of-ds")]
         [c (value-of-ds consequent env)]
         [(not c) (value-of-ds alternative env)]))]
    [`(let ,lets ,body)
     (cond
       [(null? lets) (value-of-ds body env)]
       [else
        (let ([sym (car (car lets))]
              [defined-as (value-of-ds (car (cdr (car lets))) env)])
          (value-of-ds (list 'let (cdr lets) body) (extend-env-ds sym defined-as env)))])]
    [`(lambda (,x) ,body)
     (lambda ([a : Val]) (value-of-ds body (extend-env-ds x a env)))]
    [`(,rator ,rand)
     (let ([clos (value-of-ds rator env)]
           [a (value-of-ds rand env)])
       (cond
         [(procedure? clos) (clos a)]
         [else (error "Your rator" clos "is not a function in value-of-ds")]))]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; value-of-ds Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test-runner
 > (value-of-ds
   '((lambda (x) (if (zero? x)
                     #t
                     #f))
     0)
   (empty-env-ds))
#t                  
> (value-of-ds
   '((lambda (x) (if (zero? x) 
                     12 
                     47)) 
     0) 
   (empty-env-ds))
12    
> (value-of-ds
   '(let ([y (* 3 4)])
      ((lambda (x) (* x y)) (sub1 6)))
   (empty-env-ds))
60
> (value-of-ds
   '(let ([x (* 2 3)])
      (let ([y (sub1 x)])
        (* x y)))
   (empty-env-ds))
30
> (value-of-ds
   '(let ([x (* 2 3)])
      (let ([x (sub1 x)])
        (* x x)))
   (empty-env-ds))
25
> (value-of-ds
   '(((lambda (f)
        (lambda (n) (if (zero? n) 1 (* n ((f f) (sub1 n))))))
      (lambda (f)
        (lambda (n) (if (zero? n) 1 (* n ((f f) (sub1 n)))))))
     5)
   (empty-env-ds))
120)
