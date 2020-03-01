#lang racket
(require rackunit)

(define-syntax test-runner
  (syntax-rules (>)
    [(_) "All tests passed!"]
    [(_ > test result more ...)
     (begin (check-equal? test 'result)
            (test-runner more ...))]))


(define last-non-zero
  (lambda (ls)
    (let/cc k
      (letrec
          ((last-non-zero
            (lambda (ls)
              (cond
                [(null? ls) ls]
                [(zero? (car ls)) (k (last-non-zero (cdr ls)))]
                [else (cons (car ls) (last-non-zero (cdr ls)))]))))
        (last-non-zero ls)))))

(test-runner
 > (last-non-zero '(0))
 ()
 > (last-non-zero '(1 2 3 0 4 5))
 (4 5)
 > (last-non-zero '(1 0 2 3 0 4 5))
 (4 5)
 > (last-non-zero '(1 2 3 4 5))
 (1 2 3 4 5))

(define my-lnz-data
  '((() . ())
    ((0 1 1 1 1 1 1 1 1) . (1 1 1 1 1 1 1 1))
    ((1) . (1))
    ((0 0 0 0 0 0 0 0 0) . ())
    ((0 1 0 1 0 1 0 1 0 1) . (1))
    ((0 1 0 1 0 1 0 1 0 1 0) . ())
    ((1 3 4 2 56 1234 23 4 42 3 2 0) . ())))

(for-each
 (lambda (p) (check-equal? (last-non-zero (car p))
                           (cdr p)))
 my-lnz-data)