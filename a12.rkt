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

(displayln "\nProblem 1")
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

(displayln "\nProblem 2")
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

(displayln "\nProblem 3")
(run-writer (powerXpartials 2 6))
(run-writer (powerXpartials 3 5))
(run-writer (powerXpartials 5 7))


; Problem 4
(define (replace-with-count s tr)
  (match tr
    [`,y #:when (symbol? y)
         (go-on ([dan-friedman (get)]
                 [weixi-ma (put (if (eqv? s y)
                                    (add1 dan-friedman)
                                    dan-friedman))])
           (inj-state (if (eqv? s y) dan-friedman y)))]
    [`(,a . ,d)
     (go-on ([aa (replace-with-count s a)]
             [dd (replace-with-count s d)])
       (inj-state `(,aa . ,dd)))]))

(displayln "\nProblem 4")
((run-state (replace-with-count 'o '(a o (t o (e o t ((n . m) . o) . f) . t) . r))) 0)
((run-state (replace-with-count 'o '(((h (i s . o) . a) o s o e . n) . m))) 0)
((run-state (replace-with-count 'o '(o (h (o s . o) . o) . o))) 1)


; Problem 5
(define traverse
    (lambda (inj bind f)
      (letrec
        ((trav
           (lambda (tree)
             (cond
               [(pair? tree)
                (go-on ([a (trav (car tree))]
                        [d (trav (cdr tree))])
                  (inj (cons a d)))]
               [else (f tree)]))))
        trav)))

(define (reciprocal n)
  (if (zero? n)
      (Nothing)
      (Just (/ 1 n))))
(displayln "\nProblem 5")
(reciprocal 0)
(reciprocal 2)

(define traverse-reciprocal
    (traverse Just bind-maybe reciprocal))

(traverse-reciprocal '((1 . 2) . (3 . (4 . 5))))
(traverse-reciprocal '((1 . 2) . (0 . (4 . 5))))


; Problem 6
(define (halve n)
  (cond
    [(even? n) (inj-writer (/ n 2))]
    [else (go-on ([dan-friedman (tell n)])
            (inj-writer n))]))

(displayln "\nProblem 6")
(run-writer (halve 6))
(run-writer (halve 5))

(define traverse-halve
    (traverse inj-writer bind-writer halve))
 
(run-writer (traverse-halve '((1 . 2) . (3 . (4 . 5)))))


; Problem 7
(define (state/sum n)
  (go-on ([dan-friedman (get)]
          [kanye-east (put (+ dan-friedman n))])
    (inj-state dan-friedman)))

(displayln "\nProblem 7")
((run-state (state/sum 5)) 0)
((run-state (state/sum 2)) 0)
((run-state (state/sum 2)) 3)

(displayln "")
