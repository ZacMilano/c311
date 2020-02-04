#lang typed/racket
(require typed/rackunit)

; Problem 1
(: countdown (-> Number (Listof Number)))
(define (countdown n)
  (cond
    [(= n 0) '(0)]
    [else (cons n (countdown (sub1 n)))]))

(check-equal? (countdown 0)
              '(0))
(check-equal? (countdown 5)
              '(5 4 3 2 1 0))
(check-equal? (countdown 8)
              '(8 7 6 5 4 3 2 1 0))

; Problem 2
(: insertR (-> Symbol Symbol (Listof Symbol) (Listof Symbol)))
(define (insertR look-for add-after orig-list)
  (cond
    [(null? orig-list) '()]
    [(eqv? (car orig-list) look-for)
     (cons (car orig-list)
           (cons add-after
                 (insertR look-for
                          add-after
                          (cdr orig-list))))]
    [else (cons (car orig-list) (insertR look-for add-after (cdr orig-list)))]))

(check-equal? (insertR 'a 'b '())
              '())
(check-equal? (insertR 'x 'y '(x z z x y x))
              '(x y z z x y y x y))
(check-equal? (insertR 'z 'b '(z))
              '(z b))
(check-equal? (insertR 'z 'b '(b))
              '(b))
(check-equal? (insertR 'a 'a '(a b c))
              '(a a b c))

; Problem 3
(: remv-1st (-> Symbol (Listof Symbol) (Listof Symbol)))
(define (remv-1st rmv lst)
  (cond
    [(null? lst) '()]
    [(eqv? (car lst) rmv) (cdr lst)]
    [else (cons (car lst) (remv-1st rmv (cdr lst)))]))

(check-equal? (remv-1st 'x '(x y z x))
              '(y z x))
(check-equal? (remv-1st 'y '(x y z y x))
              '(x z y x))
(check-equal? (remv-1st 'z '(a b c))
              '(a b c))
(check-equal? (remv-1st 'q '())
              '())
(check-equal? (remv-1st 'd '(d d d))
              '(d d))

; Problem 4
(: list-index-ofv (-> Symbol (Listof Symbol) Number))
(define (list-index-ofv find lst)
  (cond
    [(null? list) (error "Bad data")]
    [(eqv? (car lst) find) 0]
    [else (add1 (list-index-ofv find (cdr lst)))]))


(check-equal? (list-index-ofv 'x '(x y z x x))
              0)
(check-equal? (list-index-ofv 'x '(y z x x))
              2)
(check-equal? (list-index-ofv 'a '(b c d f e g a))
              6)

; Problem 5
(: filter (All (A) (-> (-> A Boolean) (Listof A) (Listof A))))
(define (filter pred lst)
  (cond
    [(null? lst) '()]
    [(pred (car lst)) (cons (car lst) (filter pred (cdr lst)))]
    [else (filter pred (cdr lst))]))

(check-equal? (filter even? '(1 2 3 4 5 6))
              '(2 4 6))
(check-equal? (filter odd? '(1 3 5 7))
              '(1 3 5 7))
(check-equal? (filter null? '(() () () (1 2 3)))
              '(() () ()))
(check-equal? (filter even? '())
              '())

; Problem 6
(: zip (All (A B) (-> (Listof A) (Listof B) (Listof (Pair A B)))))
(define (zip lst1 lst2)
  (cond
    [(or (null? lst1) (null? lst2)) '()]
    [else (let ([new-pair (cons (car lst1) (car lst2))])
            (cons new-pair
                  (zip (cdr lst1) (cdr lst2))))]))

(check-equal? (zip '(1 2 3) '(a b c))
              '((1 . a) (2 . b) (3 . c)))
(check-equal? (zip '(1 2 3 4 5 6) '(a b c))
              '((1 . a) (2 . b) (3 . c)))
(check-equal? (zip '(1 2 3) '(a b c d e f))
              '((1 . a) (2 . b) (3 . c)))
(check-equal? (zip '() '())
              '())

; Problem 7
(: map (All (A B) (-> (-> A B) (Listof A) (Listof B))))
(define (map p ls)
  (cond
    [(null? ls) '()]
    [else (cons (p (car ls)) (map p (cdr ls)))]))

(check-equal? (map add1 '(1 2 3 4))
              '(2 3 4 5))
(check-equal? (map sub1 '(1 2 3 4))
              '(0 1 2 3))
(check-equal? (map add1 '())
              '())

; Problem 8
(: append (All (A B) (-> (Listof A) (Listof B) (Listof (Union A B)))))
(define (append ls1 ls2)
  (cond
    [(null? ls1) ls2]
    [else (cons (car ls1) (append (cdr ls1) ls2))]))

(check-equal? (append '(42 120) '(1 2 3))
              '(42 120 1 2 3))
(check-equal? (append '(a b c) '(cat dog))
              '(a b c cat dog))
(check-equal? (append '() '())
              '())
(check-equal? (append '() '(ayo 1 2 3))
              '(ayo 1 2 3))
(check-equal? (append '(ayo 1 2 3) '())
              '(ayo 1 2 3))

; Problem 9
(: reverse (-> (Listof Any) (Listof Any)))
(define (reverse ls)
  (cond
    [(null? ls) '()]
    [else (append (reverse (cdr ls))
                  (cons (car ls) '()))]))

(check-equal? (reverse '(a 3 x))
              '(x 3 a))
(check-equal? (reverse '())
              '())
(check-equal? (reverse '(a))
              '(a))
(check-equal? (reverse '(a b c b a))
              '(a b c b a))

; Problem 10
(: fact (-> Number Number))
(define (fact n)
  (cond
    [(= n 0) 1]
    [else (* n (fact (sub1 n)))]))

(check-equal? (fact 0)
              1)
(check-equal? (fact 5)
              120)
(check-equal? (fact 1)
              1)
(check-equal? (fact 10)
              (* 10 9 8 7 6 5 4 3 2))

; Problem 11
(: fib (-> Number Number))
(define (fib n)
  (cond
    [(or (= n 0) (= n 1)) n]
    [else (+ (fib (sub1 n))
             (fib (sub1 (sub1 n))))]))

(check-equal? (fib 0)
              0)
(check-equal? (fib 1)
              1)
(check-equal? (fib 7)
              13)
(check-equal? (fib 9)
              34)

; Problem 12
; (a b) == (a . (b . ()))
;
; ((w x) y (z)) == ((w . (x . ())) . (y . ((z . ()) . ())))

(check-equal? '((w x) y (z))
              '((w . (x . ())) . (y . ((z . ()) . ()))))

; Problem 13
; Assume that bits is in reverse bit order
(: binary->natural (-> (Listof Number) Number))
(define (binary->natural bits)
  (cond
    [(null? bits) 0]
    [else (+ (car bits) (* 2 (binary->natural (cdr bits))))]))

(check-equal? (binary->natural '())
              0)
(check-equal? (binary->natural '(0 0 1))
              4)
(check-equal? (binary->natural '(0 0 1 1))
              12)
(check-equal? (binary->natural '(1 1 1 1))
              15)
(check-equal? (binary->natural '(1 0 1 0 1))
              21)
(check-equal? (binary->natural '(1 1 1 1 1 1 1 1 1 1 1 1 1))
              8191)
(check-equal? (binary->natural '(0 0 0 0 0 0 0 0 0 0 0 0 0 1))
              8192)

; Problem 14
(: minus (-> Number Number Number))
; m - n
; Assume m >= n >= 0
(define (minus m n)
  (cond
    [(= n 0) m]
    [else (sub1 (minus m (sub1 n)))]))

(check-equal? (minus 5 3)
              2)
(check-equal? (minus 100 50)
              50)
(check-equal? (minus 123 123)
              0)
(check-equal? (minus 0 0)
              0)
(check-equal? (minus 5 0)
              5)

; Problem 15
(: div (-> Number Number Number))
; m / n
; Assume n | m (n divides m)
(define (div m n)
  (cond
    [(= m n) 1]
    [else (add1 (div (minus m n) n))]))

(check-equal? (div 25 5)
               5)
(check-equal? (div 36 6)
               6)
(check-equal? (div 38 38)
               1)
(check-equal? (div 13 1)
               13)

; Problem 16
(: append-map (All (A B) (-> (-> A (Listof B)) (Listof A) (Listof B))))
(define (append-map p ls)
  (cond
    [(null? ls) '()]
    [else (append (p (car ls))
                  (append-map p (cdr ls)))]))

(check-equal? (append-map countdown (countdown 5))
              '(5 4 3 2 1 0 4 3 2 1 0 3 2 1 0 2 1 0 1 0 0))
(check-equal? (append-map countdown '())
              '())
(check-equal? (append-map countdown '(0 0 0))
              '(0 0 0))
(check-equal? (append-map countdown '(0 0 0 1))
              '(0 0 0 1 0))

; Problem 17
; Remade this to fit more than the requirements given in problem 3 (ie type generality)
(: remv-first (All (A) (-> A (Listof A) (Listof A))))
(define (remv-first rmv lst)
  (cond
    [(null? lst) '()]
    [(eqv? (car lst) rmv) (cdr lst)]
    [else (cons (car lst) (remv-first rmv (cdr lst)))]))

(check-equal? (remv-first 'x '(x y z x))
              '(y z x))
(check-equal? (remv-first 'y '(x y z y x))
              '(x z y x))
(check-equal? (remv-first 'z '(a b c))
              '(a b c))
(check-equal? (remv-first 'q '())
              '())
(check-equal? (remv-first 'd '(d d d))
              '(d d))

(: set-difference (All (A B) (-> (Listof A) (Listof B) (Listof (Union A B)))))
; a - b
(define (set-difference a b)
  (cond
    [(null? b) a]
    [else (set-difference (remv-first (car b) a) (cdr b))]))

(check-equal? (set-difference '(1 2 3 4 5) '(2 4 6 8))
              '(1 3 5))
(check-equal? (set-difference '() '())
              '())
(check-equal? (set-difference '() '(ayo 1 2 3))
              '())
(check-equal? (set-difference '(1 2 3 4) '())
              '(1 2 3 4))
(check-equal? (set-difference '(1 2 3 4) '(5 6 7 8))
              '(1 2 3 4))

;; Brainteasers

; Problem 18
;(: powerset (All (A) (-> (Listof A) (Listof (Listof A)))))
;(define (powerset elements)
;  ())
;
;
;(check-equal? (powerset '(3 2 1))
;              '((3 2 1) (3 2) (3 1) (3) (2 1) (2) (1) ()))