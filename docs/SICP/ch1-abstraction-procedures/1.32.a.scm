; Show that `sum` and `product` (Exercise 1.31) are both
; special cases of a still more general notion called 
; `accumulate` that combines a collection of terms, 
; using some general accumulation function
; """"""""""""""
; (accumulate combiner null-value term a next b)
; """"""""""""""
; accumulate takes as arguments the same term and range 
; specifications as `sum` and `product`, together with
; a `combiner` procedure (of two arguments) that specifies 
; how the current term is to be combined with the accumulation 
; of the preceding terms and a `null-value` that specifies what 
; base value to use when the terms run out. Write accumulate 
; and show how `sum` and `product` can both be defined as 
; simple calls to `accumulate`.

(define (accumulate combiner null-value term a next b)
    (if (> a b) null-value
        (combiner (term a)
                  (accumulate combiner null-value term (next a) next b))))

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