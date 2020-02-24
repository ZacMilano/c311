#lang racket


; Ordered rules of CPS:
; 1. Update names
; 2. Use lets to move the serious calls to tail position
; 3. Add a 'k' parameter to every lambda
; 4. Rewrite lets using lambdas. Note that every function now takes an extra argument
; 5. Return the simple values by applying k to them. Note that lambdas are simple.


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

