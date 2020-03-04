#lang racket
(require rackunit)
(require racket/trace)

(define-syntax test-runner
  (syntax-rules (>)
    [(_) "All tests passed!"]
    [(_ > test result more ...)
     (begin (check-equal? test 'result)
            (test-runner more ...))]))


(define last-non-zero
  (lambda (ls)
    (let/cc k
      (letrec
          ((last-non-zero
            (lambda (ls)
              (cond
                [(null? ls) ls]
                [(zero? (car ls)) (k (last-non-zero (cdr ls)))]
                [else (cons (car ls) (last-non-zero (cdr ls)))]))))
        (last-non-zero ls)))))

(test-runner
 > (last-non-zero '(0))
 ()
 > (last-non-zero '(1 2 3 0 4 5))
 (4 5)
 > (last-non-zero '(1 0 2 3 0 4 5))
 (4 5)
 > (last-non-zero '(1 2 3 4 5))
 (1 2 3 4 5))

(define my-lnz-data
  '((() . ())
    ((0 1 1 1 1 1 1 1 1) . (1 1 1 1 1 1 1 1))
    ((1) . (1))
    ((0 0 0 0 0 0 0 0 0) . ())
    ((0 1 0 1 0 1 0 1 0 1) . (1))
    ((0 1 0 1 0 1 0 1 0 1 0) . ())
    ((1 3 4 2 56 1234 23 4 42 3 2 0) . ())))

(for-each
 (lambda (p) (check-equal? (last-non-zero (car p))
                           (cdr p)))
 my-lnz-data)



(define (lex exp env)
  (match exp
    [`,y #:when (symbol? y) (list 'var (search-env env y))]
    [`(zero? ,nexp) `(zero ,(lex nexp env))]
    [`(* ,nexp1 ,nexp2) `(mult ,(lex nexp1 env) ,(lex nexp2 env))]
    [`(letcc ,kvar ,body) `(letcc ,(lex body (cons kvar env)))]
    [`(throw ,kexp ,vexp) `(throw ,(lex kexp env) ,(lex vexp env))]
    [`(lambda (,x) ,body)
     (list 'lambda (lex body (cons x env)))]
    [`(,rator ,rand)
     `(app ,(lex rator env) ,(lex rand env))
     #;(list (lex rator env)
             (lex rand env))]))

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

(test-runner
 > (lex '(lambda (x) (x (lambda (y) (x y)))) '())
 (lambda (app
          (var 0)
          (lambda (app (var 1) (var 0)))))
 > (lex '(lambda (x) (x (lambda (y) (x (* y x))))) '())
 (lambda (app
          (var 0)
          (lambda (app
                   (var 1)
                   (mult (var 0) (var 1))))))
 > (lex '(lambda (x) (x (lambda (y) (zero? (x (* y x)))))) '())
 (lambda (app (var 0) (lambda (zero (app (var 1) (mult (var 0) (var 1)))))))
 > (lex '(lambda (x) (x (lambda (y) (zero? (x (* y (lambda (z) (lambda (x) (x (* z y)))))))))) '())
 (lambda (app
          (var 0)
          (lambda (zero
                   (app (var 1) (mult (var 0) (lambda (lambda (app (var 0) (mult (var 1)
                                                                                 (var 2)))))))))))
> (lex '(letcc k (lambda (x)
                   (x (lambda (y) (zero? (x (* y (lambda (z)
                                                   (lambda (x) (throw k(x (* z y)))))))))))) '())
(letcc
  (lambda (app (var 0) (lambda (zero (app (var 1) (mult (var 0) (lambda (lambda (throw (var 4)
                                                                                       (app (var 0)
                                 (mult (var 1) (var 2)))))))))))))
)


; Problem 3
(define value-of-cps
  (lambda (expr env-cps k)
    (match expr
      [`(const ,expr) (k expr)]
      [`(mult ,x1 ,x2)
       (value-of-cps
        x1 env-cps
        (lambda (n1)
          (value-of-cps
           x2 env-cps
           (lambda (n2) (k (* n1 n2))))))]
      [`(sub1 ,x)
       (value-of-cps
        x env-cps
        (lambda (n) (k (sub1 n))))]
      [`(zero ,x)
       (value-of-cps
        x env-cps
        (lambda (n) (k (zero? n))))]
      [`(if ,test ,conseq ,alt)
       (value-of-cps
        test env-cps
        (lambda (b)
          (if b
              (value-of-cps conseq env-cps k)
              (value-of-cps alt env-cps k))))]
      [`(letcc ,body)
       (value-of-cps
        body
        (lambda (y k^)
          (if (zero? y)
              (k^ k)
              (env-cps (sub1 y) k^)))
        k)]
      [`(throw ,k-exp ,v-exp)
       (value-of-cps
        k-exp env-cps
        (lambda (ke)
          (value-of-cps
           v-exp env-cps
           ; Notice that we ignore the given k here
           (lambda (ve)
             (ke ve)))))]
      [`(let ,e ,body)
       (value-of-cps
        e env-cps
        (lambda (a)
          (value-of-cps
           body (lambda (y k^)
                  (if (zero? y)
                      (k^ a)
                      (env-cps (sub1 y) k^)))
           k)))]
      [`(var ,y)
       (env-cps y k)]
      [`(lambda ,body)
       ; lambdas are simple!!! I should have listened sooner!
       (k (lambda (arg k^)
            (value-of-cps
             body (lambda (y k^)
                    (if (zero? y)
                        (k^ arg)
                        (env-cps (sub1 y) k^)))
             k^)))]
      [`(app ,rator ,rand)
       (value-of-cps
        rator env-cps
        (lambda (c-cps)
          (value-of-cps
           rand env-cps
           (lambda (ran)
             (c-cps ran k)))))])))

;(trace value-of-cps)


; for debugging
(define (d exp)
  (value-of-cps exp (empty-env) (empty-k)))

(define empty-env
  (lambda ()
    (lambda (y k^)
      (error 'value-of-cps "unbound identifier"))))
 
(define empty-k
  (lambda ()
    (lambda (v)
      v)))

; Copied tests when everything worked as expected
(test-runner
> (d '(mult (const 1)
            (mult (const 3)
                  (letcc (mult (const 2)
                               (throw (var 0) (const 4)))))))
12
> (d '(letcc (mult (const 4)
                   (throw (var 0)
                        (const 4)))))
4
> (d '(letcc (throw (var 0) (mult (const 4)
        (const 4)))))
16
> (d '(mult (const 1) (mult (const 3) (mult (const 2)(const 4)))))
24
> (d '(letcc (throw (var 0)
                  (mult (const 4)
                        (const 4)))))
16
> (d '(letcc (throw (var 0) (sub1 (mult (const 4)
        (const 4))))))
15
> (d '(mult (const 1) (mult (const 3) (mult (const 2)(const 4)))))
24
> (d '(mult (const 1) (mult (sub1 (const 3)) (mult (const 2) (sub1 (sub1 (const 4)))))))
8
> (d '(if (zero (app (lambda (let (const 50) (mult (var 0) (var 1)))) (const 0)))
          (letcc (mult (const 1)
                       (mult (const 2)
                             (mult (app (lambda (if (zero (var 0))
                                                    (const 0)
                                                    (const 5000)))
                                        (const 0))
                                   (mult (throw (var 0) (const 4))
                                         (const 5))))))
          (const 4)))
4
> (d '(if (zero (app (lambda (let (const 50) (mult (var 0) (var 1)))) (const 0)))
          (letcc (mult (throw (var 0) (const 1))
                       (mult (const 2)
                             (mult (app (lambda (if (zero (var 0))
                                                    (const 0)
                                                    (const 5000)))
                                        (const 0))
                                   (mult (const 4)
                                         (const 5))))))
          (const 4)))
1
> (d '(if (zero (app (lambda (let (const 50) (mult (var 0) (var 1)))) (const 0)))
          (letcc (mult (const 1)
                       (mult (const 2)
                             (mult (app (lambda (if (zero (var 0))
                                                    (const 0)
                                                    (throw (var 1)
                                                           (const 5000))))
                                        (const 0))
                                   (mult (const 4)
                                         (const 5))))))
          (const 4)))
0
> (d '(if (zero (app (lambda (let (const 50) (mult (var 0) (var 1)))) (const 0)))
          (letcc (mult (const 1)
                       (mult (const 2)
                             (mult (app (lambda (if (zero (var 0))
                                                    (const 0)
                                                    (throw (var 1)
                                                           (const 5000))))
                                        (const 10))
                                   (mult (const 4)
                                         (const 5))))))
          (const 4)))
5000
> (d '(if (zero (app (lambda (let (const 50) (mult (var 0) (var 1)))) (const 10)))
          (letcc (mult (const 1)
                       (mult (const 2)
                             (mult (app (lambda (if (zero (var 0))
                                                    (const 0)
                                                    (throw (var 1)
                                                           (const 5000))))
                                        (const 10))
                                   (mult (const 4)
                                         (const 5))))))
          (const 4)))
4
> (d '(if (zero (app (lambda (let (const 50) (mult (var 0) (var 1)))) (const 10)))
          (letcc (mult (const 1)
                       (mult (const 2)
                             (mult (app (lambda (if (zero (var 0))
                                                    (const 0)
                                                    (throw (var 1)
                                                           (const 5000))))
                                        (const 10))
                                   (letcc (mult (throw (var 0) (const 4))
                                                (const 5)))))))
          (const 4)))
4
> (d '(if (zero (app (lambda (let (const 50) (mult (var 0) (var 1)))) (const 0)))
          (letcc (mult (const 1)
                       (mult (const 2)
                             (mult (app (lambda (if (zero (var 0))
                                                    (const 0)
                                                    (throw (var 1)
                                                           (const 5000))))
                                        (const 10))
                                   (letcc (mult (throw (var 0) (const 4))
                                                (const 5)))))))
          (const 4)))
5000
> (d '(if (zero (app (lambda (let (const 50) (mult (var 0) (var 1)))) (const 0)))
          (letcc (mult (const 1)
                       (mult (const 2)
                             (mult (app (lambda (if (zero (var 0))
                                                    (const 0)
                                                    (const 5000)))
                                        (const 10))
                                   (letcc (mult (throw (var 0) (const 4))
                                                (const 5)))))))
          (const 4)))
40000
 )

























