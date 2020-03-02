#lang racket
(require rackunit)

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






























