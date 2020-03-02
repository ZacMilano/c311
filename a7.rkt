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

(check-equal? (lex '(lambda (o) (lambda (r) (lambda (s) (lambda (p) (lambda (g) o))))) '())
              '(lambda (lambda (lambda (lambda (lambda (var 4)))))))
(check-equal? (lex '(lambda (x) x) '()) '(lambda (var 0)))
(check-equal? (lex '(lambda (y) (lambda (x) y)) '()) '(lambda (lambda (var 1))))
(check-equal? (lex '(lambda (y) (lambda (x) (x y))) '()) '(lambda (lambda ((var 0) (var 1)))))
(check-equal? (lex '(lambda (x) (lambda (x) (x x))) '()) '(lambda (lambda ((var 0) (var 0)))))
(check-equal? (lex '(lambda (y) ((lambda (x) (x y)) (lambda (c) (lambda (d) (y c))))) '()) 
              '(lambda ((lambda ((var 0) (var 1))) (lambda (lambda ((var 2) (var 1)))))))
(check-equal? (lex '(lambda (a) (lambda (b) (lambda (c) (lambda (a) (lambda (b) (lambda (d) (lambda (a) (lambda (e) (((((a b) c) d) e) a))))))))) '())
              '(lambda (lambda (lambda (lambda (lambda (lambda (lambda (lambda ((((((var 1) (var 3)) (var 5)) (var 2)) (var 0)) (var 1)))))))))))
(check-equal? (lex '(lambda (a) (lambda (b) (lambda (c) (lambda (w) (lambda (x) (lambda (y) ((lambda (a) (lambda (b) (lambda (c) (((((a b) c) w) x) y)))) (lambda (w) (lambda (x) (lambda (y) (((((a b) c) w) x) y))))))))))) '())
              '(lambda (lambda (lambda (lambda (lambda (lambda ((lambda (lambda (lambda ((((((var 2) (var 1)) (var 0)) (var 5)) (var 4)) (var 3))))) (lambda (lambda (lambda ((((((var 8) (var 7)) (var 6)) (var 2)) (var 1)) (var 0)))))))))))))
