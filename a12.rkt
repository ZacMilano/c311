#lang racket
(require "monads.rkt")
(require rackunit)

(define-syntax test-runner
  (syntax-rules (>)
    [(_) "All tests passed!"]
    [(_ > test result more ...)
     (begin (check-equal? test 'result)
            (test-runner more ...))]))

; Problem 1
(define (findf-maybe p ls)
  (cond
    [(null? ls) (Nothing)]
    [(p (car ls)) (Just (car ls))]
    [else (findf-maybe p (cdr ls))]))

(findf-maybe symbol? '(1 2 c))
(findf-maybe boolean? '(#f 1 2 c)) 
(findf-maybe number? '(a b c))


; Problem 2
(define (partition-writer p ls)
  (cond
    [(null? ls) (inj-writer '())]
    [(p (car ls)) (go-on
                      ([part-d (partition-writer p (cdr ls))])
                    (inj-writer (cons (car ls) part-d)))]
    [else (go-on
              ([dan-friedman (tell (car ls))]
               [part-d (partition-writer p (cdr ls))])
            (inj-writer part-d))]))

(run-writer (partition-writer even? '(1 2 3 4 5 6 7 8 9 10)))
(run-writer (partition-writer odd? '(1 2 3 4 5 6 7 8 9 10)))


; Problem 3
(define powerXpartials
  (lambda (x n)
    (cond
      [(zero? n) (inj-writer 1)]
      [(zero? (sub1 n)) (inj-writer x)]
      [(odd? n)
       (go-on ([rst (powerXpartials x (sub1 n))]
               [dan-friedman (tell rst)])
         (inj-writer (* x rst)))]
      [(even? n)
       (go-on ([y (powerXpartials x (/ n 2))]
               [dan-friedman (tell y)])
         (inj-writer (* y y)))])))

(run-writer (powerXpartials 2 6))
(run-writer (powerXpartials 3 5))
(run-writer (powerXpartials 5 7))


; Problem 4
