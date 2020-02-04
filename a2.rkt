#lang typed/racket
(require typed/rackunit)


; Problem 1
(: list-ref (All (A) (→ (Listof A) Number A)))
(define list-ref
  (lambda (ls n)
    (letrec
        ([nth-cdr : (→ Number (Listof A))
                  (lambda (n)
	            (cond
                      [(zero? n) ls]
                      [else (cdr (nth-cdr (sub1 n)))])
                    )])
      (car (nth-cdr n)))))

(check-equal? (list-ref '(a b c) 2) 'c)
(check-equal? (list-ref '(a b c) 0) 'a)
(check-equal? (list-ref '(5 4 3 2 1 0) 5) 0)
(check-equal? (list-ref '(5 4 3 2 1 0) 4) 1)
(check-equal? (list-ref '(5 4 3 2 1 0) 3) 2)
(check-equal? (list-ref '(5 4 3 2 1 0) 2) 3)
(check-equal? (list-ref '(5 4 3 2 1 0) 1) 4)
(check-equal? (list-ref '(5 4 3 2 1 0) 0) 5)


; Problem 2
(: union (All (A) (-> (Listof A) (Listof A) (Listof A))))
(define (union a b)
  (cond
    [(null? a) b]
    [(memv (car a) b) (union (cdr a) b)]
    [else (cons (car a) (union (cdr a) b))]))

(check-equal? (union '() '()) '())
(check-equal? (union '(a) '()) '(a))
(check-equal? (union '() '(b)) '(b))
(check-equal? (union '(a b c) '()) '(a b c))
(check-equal? (union '() '(b c d)) '(b c d))
(check-equal? (union '(a b c) '(a b c)) '(a b c))
(check-equal? (union '(a b) '(b c)) '(a b c))
(check-equal? (union '(a b) '(c d)) '(a b c d))


; Problem 3
(: extend (All (A) (-> Number (-> A Boolean) (-> A Boolean))))
(define (extend x pred)
  (λ (y)
    (or (eqv? x y)
        (pred y))))

(check-equal? ((extend 1 even?) 0) #t)
(check-equal? ((extend 1 even?) 1) #t)
(check-equal? ((extend 1 even?) 2) #t)
(check-equal? ((extend 1 even?) 3) #f)
(check-equal? (filter (extend 1 even?) '(0 1 2 3 4 5)) '(0 1 2 4))
(check-equal? (filter (extend 3 (extend 1 even?)) '(0 1 2 3 4 5)) '(0 1 2 3 4))
(check-equal? (filter (extend 7 (extend 3 (extend 1 even?))) '(0 1 2 3 4 5)) '(0 1 2 3 4))
(check-equal? (filter (extend 7 (extend 3 (extend 1 even?))) '(0 1 2 3 7 5)) '(0 1 2 3 7))
(check-equal? (filter (extend 7 (extend 3 (extend 1 even?))) '(0 2 4 6)) '(0 2 4 6))

; Problem 4
(define-type AssociationList
  (Listof (Pairof Symbol Any)))

(: walk-symbol (-> Symbol AssociationList Any))
(define (walk-symbol x s)
  (let ([val : Any (assv x s)])
    (cond
      [(eqv? val #f) x]
      [(and (pair? val)
            (symbol? (cdr val)))
       (walk-symbol (cdr val) s)]
      [(pair? val) (cdr val)])))

(check-equal? (walk-symbol 'a '((a . 5))) 5)
(check-equal? (walk-symbol 'a '((b . c) (a . b))) 'c)
(check-equal? (walk-symbol 'a '((a . 5) (b . 6) (c . a))) 5)
(check-equal? (walk-symbol 'c '((a . 5) (b . (a . c)) (c . a))) 5)
(check-equal? (walk-symbol 'b '((a . 5) (b . ((c . a))) (c . a))) '((c . a)))
(check-equal? (walk-symbol 'd '((a . 5) (b . (1 2)) (c . a) (e . c) (d . e))) 5)
(check-equal? (walk-symbol 'd '((a . 5) (b . 6) (c . f) (e . c) (d . e))) 'f)
(check-equal? (walk-symbol 'p '((q . r) (u . z) (r . s) (t . s) (s . u) (p . q))) 'z)

; Problem 5
(define-type Exp
  (U Symbol
     (List 'lambda (List Symbol) Exp)
     (List Exp Exp)))

(: lambda->lumbda (-> Exp Any))
(define (lambda->lumbda exp)
  (match exp
    [`,s #:when (symbol? s) s]
    [`(lambda (,x) ,body) (list 'lumbda `(,x) (lambda->lumbda body))]
    [`(,rator ,rand) (list (lambda->lumbda rator) (lambda->lumbda rand))]))

(check-equal? (lambda->lumbda 'x) 'x)
(check-equal? (lambda->lumbda '(lambda (x) x))
              '(lumbda (x) x))
(check-equal? (lambda->lumbda '(lambda (z) ((lambda (y) (a z)) (h (lambda (x) (h a))))))
              '(lumbda (z) ((lumbda (y) (a z)) (h (lumbda (x) (h a))))))
(check-equal? (lambda->lumbda '(lambda (lambda) lambda))
              '(lumbda (lambda) lambda))
(check-equal? (lambda->lumbda '((lambda (lambda) lambda) (lambda (y) y)))
              '((lumbda (lambda) lambda) (lumbda (y) y)))
(check-equal? (lambda->lumbda '((lambda (x) x) (lambda (x) x)))
              '((lumbda (x) x) (lumbda (x) x)))



; Problem 6
(: var-occurs? (-> Symbol Exp Boolean))
(define (var-occurs? var exp)
  (match exp
    [`,s #:when (symbol? s) (eqv? exp var)]
    [`(lambda (,x) ,body) (and (not (eqv? x var))
                               (var-occurs? var body))]
    [`(,rator ,rand) (or (var-occurs? var rator)
                         (var-occurs? var rand))]))

(check-equal? (var-occurs? 'x 'x) #t)
(check-equal? (var-occurs? 'x '(lambda (x) y)) #f)
(check-equal? (var-occurs? 'x '(lambda (y) x)) #t)
(check-equal? (var-occurs? 'x '((z y) x)) #t)
(check-equal? (var-occurs? 'z '(lambda (x) (y (lambda (p) p)))) #f)
(check-equal? (var-occurs? 'z '(lambda (x) (y (lambda (z) p)))) #f)
(check-equal? (var-occurs? 'z '(lambda (x) (y (lambda (p) z)))) #t)


; Problem 7
(: vars (-> Exp (Listof Symbol)))
(define (vars exp)
  (match exp
    [`,s #:when (symbol? s) `(,s)]
    [`(lambda (,x) ,body) (vars body)]
    [`(,rator ,rand) (append (vars rator) (vars rand))]))

(check-equal? (vars 'x) '(x))
(check-equal? (vars '(lambda (x) x)) '(x))
(check-equal? (vars '((lambda (y) (x x)) (x y))) '(x x x y))
(check-equal? (vars '(lambda (z) ((lambda (y) (a z))
                      (h (lambda (x) (h a))))))
              '(a z h h a))
(check-equal? (vars '(lambda (x) y)) '(y))


; Problem 8
(: unique-vars (-> Exp (Listof Symbol)))
(define (unique-vars exp)
  (match exp
    [`,s #:when (symbol? s) `(,s)]
    [`(lambda (,x) ,body) (unique-vars body)]
    [`(,rator ,rand) (union (unique-vars rator) (unique-vars rand))]))

(check-equal? (unique-vars 'x) '(x))
(check-equal? (unique-vars '(lambda (x) x)) '(x))
(check-equal? (unique-vars '((lambda (y) (x x)) (x y))) '(x y))
(check-equal? (unique-vars '(lambda (z) ((lambda (y) (a z))
                      (h (lambda (x) (h a))))))
              '(z h a))
(check-equal? (unique-vars '((lambda (z) (lambda (y) (z y))) x))
              '(z y x))
(check-equal? (unique-vars '((lambda (a) (a b)) ((lambda (c) (a c)) (b a))))
              '(c b a))
(check-equal? (unique-vars '(lambda (x) y)) '(y))


; Problem 9
(: var-occurs-free? (-> Symbol Exp Boolean))
(define (var-occurs-free? var exp)
  (match exp
    [`,s #:when (symbol? s) (eqv? s var)]
    [`(lambda (,x) ,body) (and (not (eqv? x var))
                               (var-occurs-free? var body))]
    [`(,rator ,rand)
     (or (var-occurs-free? var rator)
         (var-occurs-free? var rand))]))

(check-equal? (var-occurs-free? 'x 'x) #t)
(check-equal? (var-occurs-free? 'x '(lambda (y) y)) #f)
(check-equal? (var-occurs-free? 'x '(lambda (x) (x y))) #f)
(check-equal? (var-occurs-free? 'x '(lambda (x) (lambda (x) x))) #f)
(check-equal? (var-occurs-free? 'y '(lambda (x) (x y))) #t)
(check-equal? (var-occurs-free? 'y '((lambda (y) (x y)) (lambda (x) (x y)))) #t)
(check-equal? (var-occurs-free? 'x '((lambda (x) (x x)) (x x))) #t)
(check-equal? (var-occurs-free? 'x '(lambda (x) (x y))) #f)


; Problem 10
(: var-occurs-bound? (-> Symbol Exp Boolean))
(define (var-occurs-bound? var exp)
  (match exp
    [`,s #:when (symbol? s) #f]
    [`(lambda (,x) ,body)
     (or (and (eqv? x var)
              (var-occurs-free? var body))
         (var-occurs-bound? var body))]
    [`(,rator ,rand)
     (or (var-occurs-bound? var rator)
         (var-occurs-bound? var rand))]))

(check-equal? (var-occurs-bound? 'x 'x) #f)
(check-equal? (var-occurs-bound? 'x '(lambda (x) x)) #t)
(check-equal? (var-occurs-bound? 'y '(lambda (x) x)) #f)
(check-equal? (var-occurs-bound? 'x '((lambda (x) (x x)) (x x))) #t)
(check-equal? (var-occurs-bound? 'z '(lambda (y) (lambda (x) (y z)))) #f)
(check-equal? (var-occurs-bound? 'z '(lambda (y) (lambda (z) (y z)))) #t)
(check-equal? (var-occurs-bound? 'x '(lambda (x) y)) #f)
(check-equal? (var-occurs-bound? 'x '(lambda (x) (lambda (x) x))) #t)
(check-equal? (var-occurs-bound? 'y '((lambda (x) (x y)) (lambda (y) (x y)))) #t)
(check-equal? (var-occurs-bound? 'y '((lambda (x) (x y)) (lambda (y) (x x)))) #f)
(check-equal? (var-occurs-bound? 'y '((lambda (x) (x y)) (lambda (y) (x (lambda (z) (x y)))))) #t)


; Problem 11
(: unique-free-vars (-> Exp (Listof Symbol)))
(define (unique-free-vars exp)
  (filter (λ ([v : Symbol]) (var-occurs-free? v exp))
          (unique-vars exp)))

(check-equal? (unique-free-vars 'x) '(x))
(check-equal? (unique-free-vars '(lambda (x) (x y))) '(y))
(check-equal? (unique-free-vars '((lambda (x) ((x y) e)) (lambda (c) (x (lambda (x) (x (e c)))))))
              '(y x e))
(check-equal? (unique-free-vars '(lambda (x) z))
              '(z))
(check-equal? (unique-free-vars '(lambda (x) x))
              '())
(check-equal? (unique-free-vars '(lambda (x) (y (x z))))
              '(y z))
(check-equal? (unique-free-vars '(lambda (x) (lambda (z) (y (lambda (a) (x (b (c (e (d (x (z (y (a (d (h (i (z (b (b (b (b (d (g i))))))))))))))))))))))))
              '(c e y h b d g i))


; Problem 12
(: unique-bound-vars (-> Exp (Listof Symbol)))
(define (unique-bound-vars exp)
  (filter (λ ([v : Symbol]) (var-occurs-bound? v exp))
          (unique-vars exp)))

(check-equal? (unique-bound-vars 'x) '())
(check-equal? (unique-bound-vars '(lambda (x) y)) '())
(check-equal? (unique-bound-vars '(lambda (x) (x y))) '(x))
(check-equal? (unique-bound-vars '((lambda (x) ((x y) e)) (lambda (c) (x (lambda (x) (x (e c)))))))
              '(x c))
(check-equal? (unique-bound-vars '(lambda (y) y)) '(y))
(check-equal? (unique-bound-vars '(lambda (x) (y z))) '())
(check-equal? (unique-bound-vars '(lambda (x) (lambda (x) x))) '(x))
(check-equal? (unique-bound-vars '((lambda (x)
                                     (lambda (z)
                                       (y (lambda (a)
                                            (x (b (c (e (d (x (z (y (a (d (h (i (z (b (b (b (b (d (g (lambda (x) x))))))))))))))))))))))))
                                   (lambda (a) (a z))))
              '(x a z))


; Problem 13
(: search-env (-> (Listof Symbol) Symbol Number))
(define (search-env env var)
  (cond
    [(null? env)
     (error (string-append "what is " (symbol->string var) "?"))]
    [(eqv? (car env) var) 0]
    [else (add1 (search-env (cdr env) var))]))

(check-equal? (search-env '(a b c d) 'a) 0)
(check-equal? (search-env '(a b c d) 'c) 2)
(check-equal? (search-env '(a b c d) 'b) 1)
(check-equal? (search-env '(a b c d) 'd) 3)
(check-equal? (search-env '(a b c a) 'a) 0)
(check-equal? (search-env '(a a a a) 'a) 0)


(define-type DExp
  (U (List 'var Number)
     (List 'lambda DExp)
     (List DExp DExp)))

(: lex (-> Exp (Listof Symbol) DExp))
(define (lex exp env)
  (match exp
    [`,y #:when (symbol? y) (list 'var (search-env env y))]
    [`(lambda (,x) ,body)
     (list 'lambda (lex body (cons x env)))]
    [`(,rator ,rand)
     (list (lex rator env)
           (lex rand env))]))

(check-equal?
 (lex '(lambda (o)
         (lambda (r)
           (lambda (s)
             (lambda (p)
               (lambda (g)
                 o)))))
      '())
 '(lambda (lambda (lambda (lambda (lambda (var 4)))))))
(check-equal? (lex '(lambda (x) x) '()) '(lambda (var 0)))
(check-equal? (lex '(lambda (y) (lambda (x) y)) '()) '(lambda (lambda (var 1))))
(check-equal? (lex '(lambda (y) (lambda (x) (x y))) '()) '(lambda (lambda ((var 0) (var 1)))))
(check-equal? (lex '(lambda (x) (lambda (x) (x x))) '()) '(lambda (lambda ((var 0) (var 0)))))
(check-equal? (lex '(lambda (y) ((lambda (x) (x y)) (lambda (c) (lambda (d) (y c))))) '()) 
              '(lambda ((lambda ((var 0) (var 1))) (lambda (lambda ((var 2) (var 1)))))))
(check-equal? (lex '(lambda (a)
                      (lambda (b)
                        (lambda (c)
                          (lambda (a)
                            (lambda (b)
                              (lambda (d)
                                (lambda (a)
                                  (lambda (e)
                                    (((((a b) c) d) e) a))))))))) '())
              '(lambda
                   (lambda
                       (lambda
                           (lambda
                               (lambda
                                   (lambda
                                       (lambda
                                           (lambda
                                               ((((((var 1) (var 3)) (var 5)) (var 2)) (var 0)) (var 1)))))))))))
(check-equal? (lex '(lambda (a)
                      (lambda (b)
                        (lambda (c)
                          (lambda (w)
                            (lambda (x)
                              (lambda (y)
                                ((lambda (a)
                                   (lambda (b)
                                     (lambda (c)
                                       (((((a b) c) w) x) y))))
                                 (lambda (w)
                                   (lambda (x)
                                     (lambda (y)
                                       (((((a b) c) w) x) y))))))))))) '())
              '(lambda 
                   (lambda 
                       (lambda 
                           (lambda 
                               (lambda 
                                   (lambda 
                                       ((lambda
                                            (lambda
                                                (lambda
                                                    ((((((var 2) (var 1)) (var 0)) (var 5)) (var 4)) (var 3)))))
                                        (lambda
                                            (lambda
                                                (lambda
                                                    ((((((var 8) (var 7)) (var 6)) (var 2)) (var 1)) (var 0)))))))))))))