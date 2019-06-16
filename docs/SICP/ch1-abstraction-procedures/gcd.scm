; Euclid's algorithm
; GCD(a,b) = GCD(b,r), where r is remainder of a/b
; GCD(206,40) = GCD(40,6)
;             = GCD(6,4)
;             = GCD(4,2)
;             = GCD(2,0)
;             = 2

(define (gcd a b)
    (if (= b 0)
        a
        (gcd b (remainder a b))))

(gcd 206 40) ;Value: 2
(gcd 16 28) ;Value: 4
