; If your `product` procedure generates a recursive 
; process, write one that generates an iterative process. 
; If it generates an iterative process, write one that 
; generates a recursive process.

; Iterative version of `product`
(define (product term a next b)
    (define (iter a result)
        (if (> a b) result
            (iter (next a) (* result (term a)))))
    (iter a 1)
)

(define (factorial n)
    (define (f x) x)
    (define (next x) (+ x 1))
    (product f 1 next n))

(factorial 0) ;Value: 1
(factorial 1) ;Value: 1
(factorial 2) ;Value: 2
(factorial 5) ;Value: 120