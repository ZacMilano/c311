#lang typed/racket/no-check
(require typed/rackunit)

;(define-syntax test-runner
;  (syntax-rules (>)
;    [(_) "All tests passed!"]
;    [(_ > test result more ...)
;     (begin (check-equal? test 'result)
;            (test-runner more ...))]))

;(define-syntax simple-values-macro
;  (syntax-rules (values)
;    [(_ (values a1) (values b1)) (equal? a1 b1)]
;    [(_ (values a1 a2 ...) (values b1 b2 ...)) 
;     (and (equal? a1 b1)
;          (simple-values-macro (values a2 ...)
;                               (values b2 ...)))]))

; Problem 1
; Want to test if (car to-filter) passes pred?; if it does, cons it onto
; whatever is returned with a recursive call on its cdr; else, cons onto definite-falses
;(: filter-sps (All (A) (-> (-> Any Boolean) (Listof A) (Listof A)
;                           (Listof A))))
(define (filter-sps pred? to-filter definite-falses)
  (cond
    [(null? to-filter) (values to-filter definite-falses)]
    [else (let-values ([(trues falses) (filter-sps pred? (cdr to-filter) definite-falses)])
            (if (pred? (car to-filter))
                (values (cons (car to-filter) trues)
                        falses)
                (values trues
                        (cons (car to-filter) falses))))]))

#|

> (filter-sps even? '(1 2 3 4 5 6 7 8 9 10) '())
'(2 4 6 8 10)
'(1 3 5 7 9)
> (filter-sps odd? '(1 2 3 4 5 6 7) '())
'(1 3 5 7)
'(2 4 6)
> (filter-sps (lambda (x) (or (> x 6) (< x 2))) '(1 2 3 4 5 6 7) '())
'(1 7)
'(2 3 4 5 6)
> 

|#

; Problem 1 (part 2, the return of Problem 1)


(define filter*
  (lambda (f ls)
    (cond
      [(null? ls) '()]
      [(list? (car ls))
       (cons (filter* f (car ls)) (filter* f (cdr ls)))]
      [(f (car ls)) (cons (car ls) (filter* f (cdr ls)))]
      [else (filter* f (cdr ls))])))

#|

> (filter* even? '(1 2 3 4 5 6))
'(2 4 6)
> (filter* odd? '(1 (2 3 (4 5)) 6 7))
'(1 (3 (5)) 7)
> (filter* (lambda (x) (or (even? x) (< 7 x))) '(1 (2 3 (4 5)) 6 7 ((8 9) 10)))
'((2 (4)) 6 ((8 9) 10))
> 

|#

(define (filter*-sps pred? to-filter definite-falses)
  (cond
    [(null? to-filter) (values to-filter definite-falses)]
    [else (let-values
              ([(trues falses) (filter*-sps pred? (cdr to-filter) definite-falses)])
            (cond
              [(list? (car to-filter))
               (let-values
                   ([(car-trues car-falses)
                     (filter*-sps pred? (car to-filter) '())])
                 (values (cons car-trues trues)
                         (cons car-falses falses)))
               #;(cons (filter*-sps pred? (car to-filter))
                       trues)]
              [(pred? (car to-filter))
               (values (cons (car to-filter) trues)
                       falses)]
              [else (values trues
                            (cons (car to-filter) falses))]))]))


#|

> (filter*-sps even? '(1 2 3 4 5 6) '())
'(2 4 6)
'(1 3 5)
> (filter*-sps odd? '(1 (2 3 (4 5)) 6 7) '())
'(1 (3 (5)) 7)
'((2 (4)) 6)
> (filter*-sps (lambda (x) (or (even? x) (< 7 x))) '(1 (2 3 (4 5)) 6 7 ((8 9) 10)) '())
'((2 (4)) 6 ((8 9) 10))
'(1 (3 (5)) 7 (()))
> 

|#


; Problem 2

(define (fib-sps n store)
  (cond
    [(assv n store)
     =>
     (lambda ([p : (Pairof Number Number)]) store)]
    [(or (zero? n)
         (zero? (sub1 n)))
     (cons (cons n n) store)]
    [else (let* ([r1 (fib-sps (sub1 n) store)]
                 [r2 (fib-sps (sub1 (sub1 n)) r1)]
                 [fn-1 (cdr (assv (sub1 n) r2))]
                 [fn-2 (cdr (assv (sub1 (sub1 n)) r2))])
            (cons (cons n (+ fn-1 fn-2)) r2))]))

#|

Runs rather quickly (seems faster than O(n^2)) even for n=1000

> (fib-sps 0 '())
'((0 . 0))
> (fib-sps 1 '())
'((1 . 1))
> (fib-sps 10 '())
'((10 . 55)
  (9 . 34)
  (8 . 21)
  (7 . 13)
  (6 . 8)
  (5 . 5)
  (4 . 3)
  (3 . 2)
  (2 . 1)
  (0 . 0)
  (1 . 1))
> (fib-sps 20 '())
'((20 . 6765)
  (19 . 4181)
  (18 . 2584)
  (17 . 1597)
  (16 . 987)
  (15 . 610)
  (14 . 377)
  (13 . 233)
  (12 . 144)
  (11 . 89)
  (10 . 55)
  (9 . 34)
  (8 . 21)
  (7 . 13)
  (6 . 8)
  (5 . 5)
  (4 . 3)
  (3 . 2)
  (2 . 1)
  (0 . 0)
  (1 . 1))
> 

|#

; Problem 3

(define-syntax and*
  (syntax-rules ()
    [(_) #t]
    [(_ v1) v1]
    [(_ v1 others ...)
     (if (eqv? v1 #f) #f (and* others ...))]))


#|

> (and* )
#t
> (and* 0)
0
> (and* 1)
1
> (and* 1 #t)
#t
> (and* 1 #t #f #t)
#f
> (and* 1 2 3)
3
> (and* #f)
#f
> (and* 'a)
'a
> (and* #t #t #t #t #t #t #f)
#f
> 

|#

; Problem 4

(define-syntax list*
  (syntax-rules ()
    [(_) (raise-syntax-error "Incorrect argument-count to list*")]
    [(_ e1) e1]
    [(_ e1 others ...)
     (cons e1 (list* others ...))]))

; Have this to show me that this comes from list* (when in DrRacket)
(list* 'ayo)

#|

> (list*)
[I REMOVED THE IMAGES BTW] raise-syntax-error: arity mismatch;
 the expected number of arguments does not match the given number
  given: 1
  arguments...:
> (list* 'a 'b 'c 'd)
'(a b c . d)
> (list* 'a)
'a
> (list* 'a 'b 'c 'd 'e '())
'(a b c d e)
> 

|#


; Problem 5

(define-syntax macro-list
  (syntax-rules ()
    [(_) '()]
    [(_ a d ...) (cons a (macro-list d ...))]))

#|

> (macro-list)
'()
> (macro-list 1 'b 2 'd)
'(1 b 2 d)
> (macro-list 1 1 1 1 1 1 1)
'(1 1 1 1 1 1 1)
> (macro-list 0 '() 1 '('()) 2 '('()'()))
'(0 () 1 ('()) 2 ('() '()))
> 

|#

; Problem 6

(define-syntax mcond
  (syntax-rules (else)
    [(_) (raise-syntax-error "Incorrect argument-count to mcond*")]
    [(_ (else do-this) others ...)
        do-this]
    [(_ (condition consequent) otherwise ...)
     (if condition consequent (mcond otherwise ...))]))

#|

> (mcond
   (else 'dog))
'dog
> (mcond
    (#f #t)
    (else 'dog))
'dog
> (mcond 
    (else 'cat))
'cat
> (mcond 
    (#t #t) 
    (unbound variables))
#t
> (mcond 
    (#t #t) 
    (reeeeeeecursioooooooooon works-for-me))
#t
> (mcond 
    (#t #t) 
    (really bad syntax))
[I REMOVED THE IMAGES BTW] mcond: bad syntax in: (mcond (really bad syntax))
> (mcond 
    (#t #t) 
    (thats unfortunate))
#t
> 

|#

; Problem 7

(define-syntax copy-code
  (syntax-rules ()
    [(_ x) `(,x x)]))

(define-syntax quote-quote
  (syntax-rules ()
    [(_ e) (quote (quote e))]))

(define-syntax macro-map
  (syntax-rules ()
    [(_ mac '()) '()]
    [(_ mac '(a d ...))
     (cons (mac a)
           (macro-map mac '(d ...)))]))

#|

> (macro-map quote '((lambda (x) x) (lambda (x) (+ 2 x)) (lambda (x) 7)))
'((lambda (x) x) (lambda (x) (+ 2 x)) (lambda (x) 7))
> (macro-map copy-code '((lambda (x) x) (lambda (x) (+ 2 x)) (lambda (x) 7)))
'((#<procedure> (lambda (x) x)) (#<procedure> (lambda (x) (+ 2 x))) (#<procedure> (lambda (x) 7)))
> (macro-map quote '((trinidad and tobago) (saint vincent and the grenadines) (antigua and barbuda)))
'((trinidad and tobago) (saint vincent and the grenadines) (antigua and barbuda))
> (macro-map copy-code '((lambda (x) x) (lambda (x) (+ 2 x)) (lambda (x) 7)))
'((#<procedure> (lambda (x) x)) (#<procedure> (lambda (x) (+ 2 x))) (#<procedure> (lambda (x) 7)))
> (macro-map quote-quote '((trinidad and tobago) (saint vincent and the grenadines) (antigua and barbuda)))
'('(trinidad and tobago) '(saint vincent and the grenadines) '(antigua and barbuda))
> (macro-map copy-code '())
'()
> 

|#










































