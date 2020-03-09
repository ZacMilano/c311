#lang racket
(require rackunit)

; From a6
(define empty-k
  (lambda ()
    (let ((once-only #f))
      (lambda (v)
        (if once-only
	    (error 'empty-k "You can only invoke the empty continuation once")
	    (begin (set! once-only #t) v))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ACK ACK ACK ACK ACK ACK ACK ACK ACK ACK ACK ACK ACK ACK ACK ACK ACK ACK ACK ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define ack-orig
  (lambda (m n k)
    (cond
      [(zero? m) (k (add1 n))]
      [(zero? n) (ack-orig (sub1 m) 1 k)]
      [else (ack-orig m (sub1 n) (lambda (v) (ack-orig (sub1 m) v k)))])))

(define ack
  (lambda (m n k)
    (cond
      [(zero? m) (apply-ack-k k (add1 n))]
      [(zero? n) (ack (sub1 m) 1 k)]
      [else (ack m (sub1 n) (make-ack-k m k))])))

(define make-ack-k
  (lambda (m k)
    (lambda (v) (ack (sub1 m) v k))))

 (define apply-ack-k
  (lambda (k v)
    (match k
      [_ (k v)])))

(define ack-reg-driver
  (lambda (m n)
    (error 'ack-reg-driver "leave me alone meanie im not ready yet")
    #;
    (begin [set! ack-m m]
           [set! ack-n n]
           (ack))))

(define ack-tests-data
  (list (cons 0 0)
        (cons 1 1)
        (cons 0 1)
        (cons 1 0)
        (cons 2 0)
        (cons 3 0)
        (cons 4 0)
        (cons 3 1)
        (cons 3 2)
        (cons 3 3)
        (cons 3 4)
        (cons 3 5)
        (cons 3 6)))

(for-each
 (lambda (test-case)
   (check-equal? (ack (car test-case) (cdr test-case) (empty-k))
                 (ack-orig (car test-case) (cdr test-case) (empty-k))))
 ack-tests-data)




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

