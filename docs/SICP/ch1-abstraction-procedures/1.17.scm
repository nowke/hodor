; The exponentiation algorithms in this section are 
; based on performing exponentiation by means of repeated 
; multiplication. In a similar way, one can perform integer 
; multiplication by means of repeated addition. The following 
; multiplication procedure (in which it is assumed that our 
; language can only add, not multiply) is analogous to the 
; `expt` procedure
(define (* a b) 
    (if (= b 0)
        0
        (+ a (* a (- b 1)))))

; This algorithm takes a number of steps that is linear in b. 
; Now suppose we include, together with addition, operations `double`, 
; which doubles an integer, and `halve`, which divides an (even) integer by 2. 
; Using these, design a multiplication procedure analogous to `fast-expt` that 
; uses a logarithmic number of steps.
(define (fast-mul a b)
    (define (even? x) (= (remainder x 2) 0))
    (define (double x) (+ x x))
    (define (halve x) (/ x 2))
    (cond ((= b 0) 0)
        ((even? b) (fast-mul (double a) (halve b)))
        (else (+ a (fast-mul a (- b 1))))
    )
)
(fast-mul 2 6) ;Value: 12
(fast-mul 24 67) ;Value: 1608
(fast-mul 1 7) ;Value: 7