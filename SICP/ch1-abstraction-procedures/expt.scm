; Exponent Recursive
; requires Θ(n) steps and Θ(n) space
(define (expt b n)
    (cond ((= n 0) 1)
    (else (* b (expt b (- n 1)))))
)
(expt 2 6) ;Value: 64

; Exponent iterative
; requires Θ(n) steps and Θ(1) space
(define (expt b n)
    (define (expt-iter b prod n)
        (cond ((= n 0) prod)
            (else (expt-iter b (* b prod) (- n 1)))
        )
    )
    (expt-iter b 1 n)
)
(expt 2 6) ;Value: 64
(expt 2 0) ;Value: 1

; Exponent fast
; b^n = (b^n/2)^2    if n is even
; b^n = b * b^(n-1)  if n is odd
;
; This has  Θ(log n) growth,
; for example for n = 1000, it requires only 14 multiplications
(define (fast-exp b n)
    (define (even? n) (= (remainder n 2) 0))
    (cond ((= n 0) 1)
          ((even? n) (square (fast-exp b (/ n 2))))
          (else (* b (fast-exp b (- n 1))))
    )
)
(fast-exp 2 6) ;Value: 64
(fast-exp 2 5) ;Value: 32