; The `sum` procedure is only the simplest of a vast 
; number of similar abstractions that can be captured 
; as higher-order procedures. Write an analogous procedure 
; called `product` that returns the product of the values 
; of a function at points over a given range. Show how to 
; define `factorial` in terms of `product`. Also use `product` 
; to compute approximations to π using the formula
; π/4 = (2/3)(4/3)(4/5)(6/5)(6/7)(8/7)
(define (product term a next b)
    (if (> a b) 1
        (* (term a)
           (product term (next a) next b))))

; Define `factorial`
(define (factorial n)
    (define (f x) x)
    (define (next x) (+ x 1))
    (product f 1 next n))

(factorial 0) ;Value: 1
(factorial 1) ;Value: 1
(factorial 2) ;Value: 2
(factorial 5) ;Value: 120

(define (pi-term n)
    (cond ((even? n)
            (/ (+ n 2) (+ n 1)))
           (else (/ (+ n 1) (+ n 2)))))
(define (pi-sum n)
    (define (next x) (+ x 1))
    (product pi-term 1 next n)
)
(* (pi-sum 10) 4.0)   ;Value: 3.2751010413348074
(* (pi-sum 1000) 4.0) ;Value: 3.1431607055322663