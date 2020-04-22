#lang pie


; Problems 1-5
(claim intriguing-word Atom)
(define intriguing-word 'silence)

(claim lucky-num Nat)
(define lucky-num 2)

(claim to-go-order (Pair Nat Atom))
(define to-go-order (cons 12 'chicken-nuggets))

(claim MyFirstType U)
(define MyFirstType (Pair Nat (Pair Nat Atom)))

(claim my-thing-and-Atom (Pair MyFirstType U))
(define my-thing-and-Atom (cons (cons 1 (cons 23 'ayo)) Atom))


; Problem 6
(claim with-Nats
  (-> (-> Nat Nat
          Nat)
      (Pair Nat Nat)
      Nat))

(define with-Nats
  (lambda (bin-op pr)
    (bin-op (car pr) (cdr pr))))

(check-same Nat (with-Nats (λ (n m) n) (cons 1 2)) 1)
(check-same Nat (with-Nats (λ (n m) (add1 m)) (cons 1 2)) 3)


; Problem 7
(claim at-least-two?
  (-> Nat
      Atom))

(define at-least-two?
  (lambda (n)
    (which-Nat n 'nil (lambda (smaller) (which-Nat smaller 'nil (lambda (smaller-TWO-babyy) 't))))))

(check-same Atom (at-least-two? 0) 'nil)
(check-same Atom (at-least-two? 1) 'nil)
(check-same Atom (at-least-two? 2) 't)
(check-same Atom (at-least-two? 41) 't)


; Problem 8
(claim + (-> Nat Nat
           Nat))
(define + (λ (n m) (rec-Nat n
                     m
                     (λ (k k+m) (add1 k+m)))))
 
(claim * (-> Nat Nat
           Nat))
(define * (λ (n m) (rec-Nat n
                     0
                     (λ (k k*m) (+ m k*m)))))

(claim expt (-> Nat Nat Nat))
(define expt
  (lambda (m n)
    (rec-Nat n 1 (lambda (smaller-n m-to-n-1)
                   (* m m-to-n-1)))))

(check-same Nat (expt 1 1) 1)
(check-same Nat (expt 1 0) 1)
(check-same Nat (expt 1 123) 1)
(check-same Nat (expt 2 1) 2)
(check-same Nat (expt 2 0) 1)
(check-same Nat (expt 2 10) 1024)


; Problem 9
(claim map
  (Π ((A U)
      (B U))
    (→ (→ A B) (List A)
       (List B))))

(define map
  (lambda (A B f ls-A)
    (rec-List ls-A
      (the (List B) nil)
      (lambda (a d d-B)
        (:: (f a) d-B)))))

(check-same (List Nat) (map Nat Nat (lambda (a) (add1 a)) nil) (the (List Nat) nil))
(check-same (List Nat) (map Nat Nat (lambda (a) (add1 a)) (:: 1 nil)) (:: 2 nil))
(check-same (List Nat) (map Nat Nat (lambda (a) (* a 8)) (:: 8 (:: 16 nil))) (:: 64 (:: 128 nil)))


; Problem 10
(claim nth-cdr
  (Π ((A U))
    (-> (List A) Nat
        (List A))))

(define nth-cdr
  (lambda (A ls n)
    (rec-Nat n
      ls
      (lambda (n-1 n-1th-cdr)
        (rec-List (the (List A) n-1th-cdr)
          (the (List A) nil)
          (lambda (a d _)
            d))))))

(check-same (List Nat) (nth-cdr Nat nil 0) nil)
(check-same (List Nat) (nth-cdr Nat nil 3) nil)
(check-same (List Nat) (nth-cdr Nat (:: 1 nil) 3) nil)
(check-same (List Nat) (nth-cdr Nat (:: 1 nil) 1) nil)
(check-same (List Nat) (nth-cdr Nat (:: 1 nil) 0) (:: 1 nil))
(check-same (List Atom)
            (nth-cdr Atom (:: 'a (:: 's (:: 'd (:: 'f nil)))) 0)
            (:: 'a (:: 's (:: 'd (:: 'f nil)))))
(check-same (List Atom)
            (nth-cdr Atom (:: 'a (:: 's (:: 'd (:: 'f nil)))) 3)
            (:: 'f nil))

(claim nth
  (Π ((A U))
    (→ (List A) A Nat
       A)))

(define nth
  (lambda (A ls default n)
    (rec-List (nth-cdr A ls n)
      default
      (lambda (a d dd-get-out-of-my-laboratory)
        a))))

(check-same Nat (nth Nat nil 3 0) 3)
(check-same Nat (nth Nat nil 3 7) 3)
(check-same Nat (nth Nat (:: 1 nil) 1234 3) 1234)
(check-same Nat (nth Nat (:: 1 nil) 1234 1) 1234)
(check-same Nat (nth Nat (:: 1 nil) 1234 0) 1)
(check-same Atom
            (nth Atom (:: 'a (:: 's (:: 'd (:: 'f nil)))) 'you-have-failed 0)
            'a)
(check-same Atom
            (nth Atom (:: 'a (:: 's (:: 'd (:: 'f nil)))) 'you-have-failed 3)
            'f)
(check-same Atom
            (nth Atom (:: 'a (:: 's (:: 'd (:: 'f nil)))) 'you-have-failed 4)
            'you-have-failed)
(check-same Atom
            (nth Atom (:: 'a (:: 's (:: 'd (:: 'f nil)))) 'you-have-failed 798)
            'you-have-failed)


; Problem 11
(claim vec-second
  (Π ((A U)
      (n Nat))
    ; wow that's dope
    (-> (Vec A (add1 (add1 n)))
        A)))

(define vec-second
  (lambda (A n vecc)
    (head (tail vecc))))

(check-same Nat (vec-second Nat 0 (vec:: 1 (vec:: 8 vecnil))) 8)
(check-same Nat (vec-second Nat 2 (vec:: 3 (vec:: 7 (vec:: 1 (vec:: 8 vecnil))))) 7)
(check-same Atom (vec-second Atom 2 (vec:: 'a (vec:: 's (vec:: 'd (vec:: 'f vecnil))))) 's)

