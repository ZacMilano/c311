#lang racket
(require (only-in rackunit check-equal?))
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

(define part-1-1
  (run 2 (q)
    (== 5 q)
    (conde [(conde [(== 5 q) (== 6 q)])
            (== 5 q)]
           [(== q 5)])))

;; The value is ((5)), and thus there is only one answer. First, mk unifies q
;; with 5, and then proceeds to the outermost conde. The first case of the
;; outermost conde runs its condition, which fails, because q cannot be
;; simultaneously unified with both 5 and 6. Then, the second case (== q 5) is
;; tested, which succeeds and q is allowed to be only 5.

;; 2 What is the value of
(define part-1-2
  (run 1 (q) 
    (fresh (a b) 
      (== `(,a ,b) q)
      (absento 'tag q)
      (symbolo a))))

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

(defrel (stuttero orig-ls stut-ls)
  (conde
   [(== '() orig-ls) (== '() stut-ls)]
   [(fresh (a d res)
      (== `(,a . ,d) orig-ls)
      (== stut-ls `(,a ,a . ,res))
      (stuttero d res))]))

(defrel (assoco x ls)
  (match-let* ((`(,a . ,d) ls)
               (`(,aa . ,da) a))
    (cond
      ((equal? aa x) a)
      ((not (equal? aa x)) (assoco x d)))))

(defrel (reverseo ls)
  (cond
    ((equal? '() ls) '())
    (else
     (match-let* ((`(,a . ,d) ls)
                  (res (reverseo d)))
       (append res `(,a))))))

(test-runner
 > (run 1 q (stuttero q '(1 1 2 2 3 3)))
((1 2 3))

> (run* q (stuttero q '(1 1 2 2 3 3)))
((1 2 3))

> (run 1 q (fresh (a b c d) (== q `(,a ,b ,c ,d)) (stuttero a `(1 ,b ,c 2 3 ,d))))
(((1 2 3) 1 2 3))

> (run 1 q (fresh (a b c d) (== q `(,a ,b ,c ,d)) (stuttero `(,b 1) `(,c . ,d))))
((_0 _1 _1 (_1 1 1)))

> (run 1 q (fresh (e f g) (== q `(,e ,f ,g)) (stuttero `(,e . ,f) g)))
((_0 () (_0 _0)))

> (run 2 q (fresh (e f g) (== q `(,e ,f ,g)) (stuttero `(,e . ,f) g)))
((_0 () (_0 _0)) (_0 (_1) (_0 _0 _1 _1)))
)

#;
(test-runner
 > (run* q (assoco 'x '() q))
()

> (run* q (assoco 'x '((x . 5)) q))
((x . 5))

> (run* q (assoco 'x '((y . 6) (x . 5)) q))
((x . 5))

> (run* q (assoco 'x '((x . 6) (x . 5)) q))
((x . 6))

> (run* q (assoco 'x '((x . 5)) '(x . 5)))
(_0)

> (run* q (assoco 'x '((x . 6) (x . 5)) '(x . 6)))
(_0)

> (run* q (assoco 'x '((x . 6) (x . 5)) '(x . 5)))
()

> (run* q (assoco q '((x . 6) (x . 5)) '(x . 5)))
()

> (run* q (assoco 'x '((x . 6) . ,q) '(x . 6)))
(_0)

> (run 5 q (assoco 'x q '(x . 5)))
(((x . 5) . _0)
  ((_0 . _1) (x . 5) . _2)
  ((_0 . _1) (_2 . _3) (x . 5) . _4)
  ((_0 . _1) (_2 . _3) (_4 . _5) (x . 5) . _6)
  ((_0 . _1) (_2 . _3) (_4 . _5) (_6 . _7) (x . 5) . _8))

> (run 5 q (fresh (x y z)
                (assoco x y z)
                (== `(,x ,y ,z) q)))
((_0 ((_0 . _1) . _2) (_0 . _1))
  (_0 ((_1 . _2) (_0 . _3) . _4) (_0 . _3))
  (_0 ((_1 . _2) (_3 . _4) (_0 . _5) . _6) (_0 . _5))
  (_0 ((_1 . _2) (_3 . _4) (_5 . _6) (_0 . _7) . _8) (_0 . _7))
  (_0 ((_1 . _2) (_3 . _4) (_5 . _6) (_7 . _8) (_0 . _9) . _10) (_0 . _9)))
 )

#;
(test-runner
 > (run* q (reverseo '() q))
(())

> (run* q (reverseo '(a) q))
((a))

> (run* q (reverseo '(a b c d) q))
((d c b a))

> (run* q (fresh (x) (reverseo `(a b ,x c d) q)))
((d c _0 b a))

> (run* x (reverseo `(a b ,x d) '(d c b a)))
(c)

> (run* x (reverseo `(a b c d) `(d . ,x)))
((c b a))

> (run* q (fresh (x) (reverseo `(a b c d) `(d . (,q . ,x)))))
(c)

> (run 10 q (fresh (x y) (reverseo x y) (== `(,x ,y) q)))
((() ())
  ((_0) (_0))
  ((_0 _1) (_1 _0))
  ((_0 _1 _2) (_2 _1 _0))
  ((_0 _1 _2 _3) (_3 _2 _1 _0))
  ((_0 _1 _2 _3 _4) (_4 _3 _2 _1 _0))
  ((_0 _1 _2 _3 _4 _5) (_5 _4 _3 _2 _1 _0))
  ((_0 _1 _2 _3 _4 _5 _6) (_6 _5 _4 _3 _2 _1 _0))
  ((_0 _1 _2 _3 _4 _5 _6 _7) (_7 _6 _5 _4 _3 _2 _1 _0))
  ((_0 _1 _2 _3 _4 _5 _6 _7 _8) (_8 _7 _6 _5 _4 _3 _2 _1 _0)))
 )










