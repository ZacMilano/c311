#lang racket
(require "mk.rkt")
(require "numbers.rkt")

(define-syntax test-runner
  (syntax-rules (>)
    [(_) "All tests passed!"]
    [(_ > test result more ...)
     (begin (check-equal? test 'result)
            (test-runner more ...))]))

;; Part I Write the answers to the following problems using your
;; knowledge of miniKanren.  For each problem, explain how miniKanren
;; arrived at the answer.  You will be graded on the quality of your
;; explanation; a full explanation will require several sentences.

;; 1 What is the value of 

(run 2 (q)
  (== 5 q)
  (conde [(conde [(== 5 q) (== 6 q)])
          (== 5 q)]
         [(== q 5)]))

;; The value is ((5)), and thus there is only one answer. First, mk unifies q
;; with 5, and then proceeds to the outermost conde. The first case of the
;; outermost conde runs its condition, which fails, because q cannot be
;; simultaneously unified with both 5 and 6. Then, the second case (== q 5) is
;; tested, which succeeds and q is allowed to be only 5.

;; 2 What is the value of
(run 1 (q) 
  (fresh (a b) 
    (== `(,a ,b) q)
    (absento 'tag q)
    (symbolo a)))

;; First, a and b are freshened (or, re(?)freshed), and the list
;; (cons a (cons b '())) is associated and unified with q. Next, it is ensured
;; that any possible value of q does not contain the symbol 'tag (ie neither a
;; nor b can be 'tag). Next is is ensured that a is a symbol. So it outputs
;; '(((_0 _1))) which shows that a can be something independent of what b is. A
;; better, more precise response would be something like
;; '(((_0 _1)
;;    (=/= ((_0 tag)))
;;    (sym _0) (absento (tag _1))))
;; I learned about the more specific typing from http://io.livecode.ch/learn/webyrd/webmk



;; 3 What do the following miniKanren constraints mean?
;; a ==       Unifies the two arguments, and requires that they are the same in any permissible answer
;; b =/=      Ensures that the two arguments are not exactly the same
;; c absento  Ensures that the fist argument is not an element inside the second argument (eg list)
;; d numbero  Ensures that the given argument is a number in any valid response
;; e symbolo  Ensures that the given argument is a symbol in any valid response

;; Part II goes here.

