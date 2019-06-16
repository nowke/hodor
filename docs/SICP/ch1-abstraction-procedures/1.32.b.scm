; If your accumulate procedure generates a recursive process,
;  write one that generates an iterative process. If it 
; generates an iterative process, write one that generates 
; a recursive process

; Iterative `accumulate`
(define (accumulate combiner null-value term a next b)
    (define (iter a result)
        (if (> a b) result
            (iter (next a) (combiner result (term a))))
    )
    (iter a null-value))

(define (product term a next b)
    (accumulate * 1 term a next b))
(define (sum term a next b)
    (accumulate + 0 term a next b))

; Verify `product` by factorial
(define (factorial n)
    (define (f x) x)
    (define (next x) (+ x 1))
    (product f 1 next n))

(factorial 0) ;Value: 1
(factorial 1) ;Value: 1
(factorial 2) ;Value: 2
(factorial 5) ;Value: 120

; Verify `sum` by `pi-sum`
(define (pi-sum a b) 
    (define (pi-term x)
        (/ 1.0 (* x (+ x 2)))) 
    (define (pi-next x)
        (+ x 4))
  (sum pi-term a pi-next b))
(* 8 (pi-sum 1 1000)) ;Value: 3.139592655589783