#lang racket

; From a6
(define empty-k
  (lambda ()
    (let ((once-only #f))
      (lambda (v)
        (if once-only
	    (error 'empty-k "You can only invoke the empty continuation once")
	    (begin (set! once-only #t) v))))))

(define ack
  (lambda (m n k)
    (cond
      [(zero? m) (k (add1 n))]
      [(zero? n) (ack (sub1 m) 1 k)]
      [else (ack m (sub1 n) (lambda (v) (ack (sub1 m) v k)))])))
 
(define ack-reg-driver
  (lambda (m n)
    (error 'ack-reg-driver "leave me alone meanie im not ready yet")
    #;
    (begin [set! ack-m m]
           [set! ack-n n]
           (ack))))





(define depth
  (lambda (ls k)
    (cond
      [(null? ls) (k 1)]
      [(pair? (car ls))
       (depth (car ls)
	      (lambda (l)
		(depth (cdr ls)
		       (lambda (r)
			 (let ((l (add1 l)))
			   (if (< l r) (k r) (k l)))))))]
      [else (depth (cdr ls) k)])))

(define depth-reg-driver
  (lambda (ls k)
    (error 'depth-reg-driver "leave me alone meanie im not ready yet")
    #;
    (begin [set! depth-ls ls]
           (depth))))

