#lang racket
(require "mk.rkt")
(require (only-in rackunit check-equal?))

(define-syntax test-runner
  (syntax-rules (>)
    [(_) "All tests passed!"]
    [(_ > test result more ...)
     (begin (check-equal? test 'result)
            (test-runner more ...))]))
(define gamma-0
  '((x . Nat) (x . Bool) (z . Nat)))

(defrel (lookupo gamma x t)
  (fresh (x^ t^ gamma^)
    (== `((,x^ . ,t^) . ,gamma^) gamma)
    (conde
     [(== x^ x) (== t^ t)]
     [(=/= x^ x)
      (lookupo gamma^ x t)])))

;(run 1 t (lookupo gamma-0 'x t))
(defrel (!-- gamma e t)
  (conde
   [(== t 'Bool)
    (conde
     [(== e #f)])]))

(defrel (!- gamma e t)
  (conde
   [(symbolo e)
    (lookupo gamma e t)]
   [(fresh (x body)
      (== `(fix (lambda (,x) ,body)) e)
      (!- `((,x . ,t) . ,gamma) body t))]
   [(numbero e) (== t 'Nat)]
   [(== t 'Bool)
    (conde
     [(== e #t)]
     [(== e #f)])]
   [(fresh (e1 e2)
      (conde
       [(== `(* ,e1 ,e2) e)]
       [(== `(+ ,e1 ,e2) e)])
      (== t 'Nat)
      (!- gamma e1 'Nat)
      (!- gamma e2 'Nat))]
   [(fresh (e1 e2 e3)
      (== `(if ,e1 ,e2 ,e3) e)
      (!- gamma e1 'Bool)
      (!- gamma e2 t)
      (!- gamma e3 t))]
   [(fresh (e1)
      (== `(sub1 ,e1) e)
      (!- gamma e1 'Nat)
      (== t 'Nat))]
   [(fresh (e1)
      (== `(not ,e1) e)
      (!- gamma e1 'Bool)
      (== t 'Bool))]
   [(fresh (e1)
      (== `(zero? ,e1) e)
      (!- gamma e1 'Nat)
      (== t 'Bool))]
   [(fresh (x body)
      (== `(lambda (,x) ,body) e)
      (symbolo x)
      (fresh (input-t output-t)
        (== `(,input-t -> ,output-t) t)
        (!- `((,x . ,input-t) . ,gamma) body output-t)))]
   [(fresh (rator rand)
      (== `(,rator ,rand) e)
      (fresh (input-t)
        (!- gamma rator `(,input-t -> ,t))
        (!- gamma rand input-t)))]))


(test-runner
 > (run* q (!- '() #t q))
(Bool)
> (run* q (!- '() 17 q))
(Nat)
> (run* q (!- '() '(zero? 24) q))
(Bool)
> (run* q (!- '() '(zero? (sub1 24)) q))
(Bool)
> (run* q (!- '() '(not (zero? (sub1 24))) q))
(Bool)
> (run* q
    (!- '() '(zero? (sub1 (sub1 18))) q))
(Bool)
> (run* q
    (!- '()  '(lambda (n) (if (zero? n) n n)) q))
((Nat -> Nat))
> (run* q
    (!- '() '((lambda (n) (zero? n)) 5) q))
(Bool)
> (run* q
    (!- '() '(if (zero? 24) 3 4) q))
(Nat)
> (run* q
    (!- '() '(if (zero? 24) (zero? 3) (zero? 4)) q))
(Bool)
> (run* q
    (!- '() '(lambda (x) (sub1 x)) q))
((Nat -> Nat))
> (run* q
    (!- '() '(lambda (a) (lambda (x) (+ a x))) q))
((Nat -> (Nat -> Nat)))
> (run* q
    (!- '() '(lambda (f)
               (lambda (x)
                 ((f x) x)))
         q))
(((_0 -> (_0 -> _1)) -> (_0 -> _1)))
> (run* q
    (!- '() '(sub1 (sub1 (sub1 6))) q))
(Nat)
> (run 1 q
    (fresh (t)
      (!- '() '(lambda (f) (f f)) t)))
()
> (length (run 20 (q)
             (fresh (lam a b)
               (!- '() `((,lam (,a) ,b) 5) 'Nat)
               (== `(,lam (,a) ,b) q))))
20
> (length (run 30 q (!- '() q 'Nat)))
30
> (length (run 30 q (!- '() q '(Nat -> Nat))))
30
> (length (run 500 q (!- '() q '(Nat -> Nat))))
500
> (length (run 30 q (!- '() q '(Bool -> Nat))))
30
> (length (run 30 q (!- '() q '(Nat -> (Nat -> Nat)))))
30
> (length (run 100 q
             (fresh (e t)
               (!- '() e t)
               (== `(,e ,t) q))))
100
> (length (run 100 q
             (fresh (g e t)
               (!- g e t)
               (== `(,g ,e ,t) q))))
100
> (length
   (run 100 q
     (fresh (g v)
       (!- g `(var ,v) 'Nat)
       (== `(,g ,v) q))))
100
> (run 1 q
       (fresh (g)
	 (!- g
	      '((fix (lambda (!)
		       (lambda (n)
			 (if (zero? n)
			     1
			     (* n (! (sub1 n)))))))
		5)
	      q)))
(Nat)
> (run 1 q
       (fresh (g)
	 (!- g
	      '((fix (lambda (!)
		       (lambda (n)
			 (* n (! (sub1 n))))))
		5)
	      q)))
(Nat))
