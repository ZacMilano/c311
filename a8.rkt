#lang racket
(require rackunit)

; From a6
(define empty-k
  (lambda ()
    (lambda (v) v)))


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
  (lambda (ack-m ack-n ack-k)
    (cond
      [(zero? ack-m)
       (let* ([apply-ack-k-k ack-k]
              [apply-ack-k-v (add1 ack-n)])
         (apply-ack-k apply-ack-k-k apply-ack-k-v))]
      [(zero? ack-n)
       (let* ([ack-k ack-k]
              [ack-m (sub1 ack-m)]
              [ack-n 1])
         (ack ack-m ack-n ack-k))]
      [else
       (let* ([ack-k (make-ack-k ack-m ack-k)]
              [ack-m ack-m]
              [ack-n (sub1 ack-n)])
         (ack ack-m ack-n ack-k))])))

(define make-ack-k
  (lambda (m k)
    `(make-ack-k ,m ,k)))

 (define empty-ack-k
  (lambda ()
    `(empty-ack-k)))

 (define apply-ack-k
  (lambda (apply-ack-k-k apply-ack-k-v)
    (match apply-ack-k-k
      [`(make-ack-k ,m ,k)
       (let* ([ack-k k]
              [ack-m (sub1 m)]
              [ack-n apply-ack-k-v])
         (ack ack-m ack-n ack-k))]
      [`(empty-ack-k) apply-ack-k-v])))

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
   (check-equal? (let* ([ack-k (empty-ack-k)]
                        [ack-m (car test-case)]
                        [ack-n (cdr test-case)])
                   (ack ack-m ack-n ack-k))
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

