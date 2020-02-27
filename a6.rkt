#lang racket
(require rackunit)


; Ordered rules of CPS:
; 1. Update names
; 2. Use lets to move the serious calls to tail position
; 3. Add a 'k' parameter to every lambda
; 4. Rewrite lets using lambdas. Note that every function now takes an extra argument
; 5. Return the simple values by applying k to them. Note that lambdas are simple.

; Serious calls are the functions you defined,
; especially the recursive ones.
;
; Simple calls are those from Racket.
;
; A tail call is function calls with no immediate context.
; e.g. g in (g x) is a tail call; g in (f (g x)) is not a
; tail call, but f is.


; Initial continuation
(define empty-k
  (lambda ()
    (let ((once-only #f))
      (lambda (v)
        (if once-only
	    (error 'empty-k "You can only invoke the empty continuation once")
	    (begin (set! once-only #t) v))))))


; Problem 1
(define binary-to-decimal
  (lambda (n)
    (cond
      [(null? n) 0]
      [else (+ (car n) (* 2 (binary-to-decimal (cdr n))))])))


(define binary-to-decimal-cps
  (lambda (n k)
    (cond
      [(null? n) (k 0)]
      [else
       (binary-to-decimal-cps
        (cdr n)
        (lambda (val) (k (+ (car n) (* 2 val)))))])))


(define btd-tests-data
  '(()
    (0)
    (1)
    (0 0 0 0 0 0)
    (1 0 0 0 0 0)
    (0 0 0 0 0 1)
    (1 1 1 1 1 1)
    (1 0 1 0 1 0 1)
    (1 0 0 1 0 1 0 0 1 1 0 1 0 1 1 0 1)))

(for-each
 (lambda (ls)
   (check-equal? (binary-to-decimal ls)
                 (binary-to-decimal-cps ls (empty-k))))
 btd-tests-data)


; Problem 2
(define times
  (lambda (ls)
    (cond
      [(null? ls) 1]
      [(zero? (car ls)) 0]
      [else (* (car ls) (times (cdr ls)))])))

(define times-cps
  (lambda (ls k)
    (cond
      [(null? ls) (k 1)]
      [(zero? (car ls)) (k 0)]
      [else (times-cps (cdr ls)
                       (lambda (val) (k (* (car ls) val))))])))

(define times-tests-data
  '(()
    (3)
    (1)
    (0)
    (3 1 1 1 1)
    (3 5 3 0)
    (6 2 4 7 4 39 4 62345 4 2 1)
    (6 2 4 7 4 39 4 62345 4 2 1 0)
    (6 2 4 7 4 39 0 4 62345 4 2 1)))

(for-each
 (lambda (ls)
   (check-equal? (times ls)
                 (times-cps ls (empty-k))))
 times-tests-data)


; Problem 3
(define times-cps-shortcut
  (lambda (ls k)
    (cond
      [(null? ls) (k 1)]
      [(zero? (car ls)) k 0]
      [else (times-cps (cdr ls)
                       (lambda (val) (k (* (car ls) val))))])))

(for-each
 (lambda (ls)
   (check-equal? (times ls)
                 (times-cps-shortcut ls (empty-k))))
 times-tests-data)


; Problem 4
(define plus
  (lambda (m)
    (lambda (n)
      (+ m n))))

(define plus-cps
  (lambda (m k1)
    (k1 (lambda (n k2)
          (k2 (+ m n))))))

(define plus-tests-data
  '((0 . 0)
    (0 . 5)
    (5 . 0)
    (5 . 5)
    (10 . 1)
    (1 . 10)
    (1234 . 928382)
    (365789 . 19)))

(for-each
 (lambda (p)
   (check-equal? ((plus (car p)) (cdr p))
                 (plus-cps (car p) (lambda (f) (f (cdr p) (empty-k))))))
 plus-tests-data)


; Problem 5
(define remv-first-9*
  (lambda (ls)
    (cond
      [(null? ls) '()]
      [(pair? (car ls))
       (cond
         [(equal? (car ls) (remv-first-9* (car ls)))
          (cons (car ls) (remv-first-9* (cdr ls)))]
         [else (cons (remv-first-9* (car ls)) (cdr ls))])]
      [(eqv? (car ls) '9) (cdr ls)]
      [else (cons (car ls) (remv-first-9* (cdr ls)))])))

(define remv-first-9*-cps
  (lambda (ls k)
    (cond
      [(null? ls) (k '())]
      [(pair? (car ls))
       (remv-first-9*-cps
        (car ls)
        (lambda (v)
          (cond
            [(equal? (car ls) v)
             (remv-first-9*-cps (cdr ls)
                                (lambda (v-prime)
                                  (k (cons (car ls) v-prime))))]
            [else (k (cons v (cdr ls)))])))]
      [(eqv? (car ls) '9) (k (cdr ls))]
      [else
       (remv-first-9*-cps
        (cdr ls)
        (lambda (v)
          (k (cons (car ls) v))))])))

(define remv-first-9*-tests-data
  '(((1 2 (3) 9))
    (9 (9 (9 (9))))
    (((((9) 9) 9) 9) 9)
    ()
    (9)
    (((((((((((9)))))))))))
    ((9) (((((((()))((())(()))(()()))(())())))))
    ((9) (((((((9 ()))(9 ((9))((9) 9))(() 9 ()))(())(9))))))
    ((0) (((((((0 ()))(69 ((29))((49) 49))(() 29 ()))(())(9))))))
    ((0) (((((((0 ()))(69 ((29))((49) 49))(() 29 ()))(())(19))))))))

(for-each
 (lambda (l*)
   (check-equal? (remv-first-9* l*)
                 (remv-first-9*-cps l* (empty-k))))
 remv-first-9*-tests-data)


; Problem 6
(define cons-cell-count
  (lambda (ls)
    (cond
      [(pair? ls)
       (add1 (+ (cons-cell-count (car ls)) (cons-cell-count (cdr ls))))]
      [else 0])))

(define cons-cell-count-cps
  (lambda (ls k)
    (cond
      [(pair? ls)
       (cons-cell-count-cps
        (car ls)
        (lambda (v1)
          (cons-cell-count-cps
           (cdr ls)
           (lambda (v2)
             (k (add1 (+ v1 v2)))))))
       ]
      [else (k 0)])))

(for-each
 (lambda (l*)
   (check-equal? (cons-cell-count l*)
                 (cons-cell-count-cps l* (empty-k))))
 remv-first-9*-tests-data)


; Problem 7
(define find 
  (lambda (u s)
    (let ((pr (assv u s)))
      (if pr (find (cdr pr) s) u))))

; A Problem7TestCase is
; (make-find-test-case-data (U Number Symbol) AssociationList)
(define-struct find-test-case-data [association-list symbol])

; This seemed odd. I didn't modify k.
(define find-cps
  (lambda (u s k)
    (let ((pr (assv u s)))
      (cond
        [pr (find-cps (cdr pr) s k)]
        [else (k u)]))))

(define find-tests-data
  (list (make-find-test-case-data 5 '((5 . a) (6 . b) (7 . c)))
        (make-find-test-case-data 7 '((5 . a) (6 . 5) (7 . 6)))
        (make-find-test-case-data 5 '((5 . 6) (9 . 6) (2 . 9)))
        (make-find-test-case-data 'a '())
        (make-find-test-case-data 'a '((b . c)))
        (make-find-test-case-data 'a '((b . c) (a . d) (c . q) (d . b)))
        (make-find-test-case-data 'a '((b . c) (a . d) (c . q) (d . b) (q . 65)))))

(for-each
 (lambda (tc)
   (check-equal? (find (find-test-case-data-association-list tc)
                       (find-test-case-data-symbol tc))
                 (find-cps (find-test-case-data-association-list tc)
                           (find-test-case-data-symbol tc)
                           (empty-k))))
 find-tests-data)


; Problem 8
;; ack: computes the Ackermann function
;; (http://en.wikipedia.org/wiki/Ackermann_function). Warning: if you
;; run this program with m >= 4 and n >= 2, you'll be in for a long
;; wait.
(define ack
  (lambda (m n)
    (cond
      [(zero? m) (add1 n)]
      [(zero? n) (ack (sub1 m) 1)]
      [else (ack (sub1 m)
                 (ack m (sub1 n)))])))

(define ack-cps
  (lambda (m n k)
    (cond
      [(zero? m) (k (add1 n))]
      [(zero? n) (ack-cps (sub1 m) 1 k)]
      [else
       (ack-cps m
                (sub1 n)
                (lambda (v)
                  (ack-cps (sub1 m) v k)))
       #;(ack-cps (sub1 m)
                  (ack-cps m (sub1 n) k))])))

; A Problem8TestCase is
; (make-find-test-case-data (U Number Symbol) AssociationList)
(define-struct ack-test-case-data [m n])

(define ack-tests-data
  (list (make-ack-test-case-data 0 0)
        (make-ack-test-case-data 1 1)
        (make-ack-test-case-data 0 1)
        (make-ack-test-case-data 1 0)
        (make-ack-test-case-data 2 0)
        (make-ack-test-case-data 3 0)
        (make-ack-test-case-data 4 0)
        (make-ack-test-case-data 3 1)
        (make-ack-test-case-data 3 2)
        (make-ack-test-case-data 3 3)
        (make-ack-test-case-data 3 4)
        (make-ack-test-case-data 3 5)
        (make-ack-test-case-data 3 6)))

(for-each
 (lambda (tc)
   (check-equal? (ack (ack-test-case-data-m tc)
                      (ack-test-case-data-n tc))
                 (ack-cps (ack-test-case-data-m tc)
                          (ack-test-case-data-n tc)
                          (empty-k))))
 ack-tests-data)


; Problem 9
(define (fib n)
  ((lambda (fib)
     (fib fib n))
   (lambda (fib n)
     (cond
       [(zero? n) 0]
       [(zero? (sub1 n)) 1]
       [else (+ (fib fib (sub1 n)) (fib fib (sub1 (sub1 n))))]))))

; if the names start confusing, they stay confusing in the cps version >:)
(define (fib-cps n k)
  ((lambda (fib-cps)
     (fib-cps fib-cps n k))
   (lambda (fib-cps n k)
     (cond
       [(zero? n) (k 0)]
       [(zero? (sub1 n)) (k 1)]
       [else
        (fib-cps
         fib-cps
         (sub1 n)
         (lambda (v1)
           (fib-cps
            fib-cps
            (sub1 (sub1 n))
            (lambda (v2)
              (k (+ v1 v2))))))
        #;(+ (fib-cps fib-cps (sub1 n))
             (fib-cps fib-cps (sub1 (sub1 n))))]))))

(define (build-list n)
  (cond
    [(zero? n) '(0)]
    [else (cons n (build-list (sub1 n)))]))

(define fib-tests-data
  (build-list 25))

(for-each
 (lambda (n)
   (check-equal? (fib n)
                 (fib-cps n (empty-k))))
 fib-tests-data)




























