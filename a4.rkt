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

; 1
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
 
