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

(check-equal? (binary-to-decimal '())
              (binary-to-decimal-cps '() (empty-k)))
(check-equal? (binary-to-decimal '(0))
              (binary-to-decimal-cps '(0) (empty-k)))
(check-equal? (binary-to-decimal '(1))
              (binary-to-decimal-cps '(1) (empty-k)))
(check-equal? (binary-to-decimal '(0 0 0 0 0 0))
              (binary-to-decimal-cps '(0 0 0 0 0 0) (empty-k)))
(check-equal? (binary-to-decimal '(1 0 0 0 0 0))
              (binary-to-decimal-cps '(1 0 0 0 0 0) (empty-k)))
(check-equal? (binary-to-decimal '(0 0 0 0 0 1))
              (binary-to-decimal-cps '(0 0 0 0 0 1) (empty-k)))
(check-equal? (binary-to-decimal '(1 1 1 1 1 1))
              (binary-to-decimal-cps '(1 1 1 1 1 1) (empty-k)))
(check-equal? (binary-to-decimal '(1 0 1 0 1 0 1))
              (binary-to-decimal-cps '(1 0 1 0 1 0 1) (empty-k)))
(check-equal? (binary-to-decimal '(1 0 0 1 0 1 0 0 1 1 0 1 0 1 1 0 1))
              (binary-to-decimal-cps '(1 0 0 1 0 1 0 0 1 1 0 1 0 1 1 0 1) (empty-k)))


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

(define times-tests
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
 times-tests)
