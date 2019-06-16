; Demonstrate that the Carmichael numbers listed in 
; Footnote 1.47 really do fool the Fermat test. That is, 
; write a procedure that takes an integer n and tests 
; whether an is congruent to a modulo n for every a < n, 
; and try your procedure on the given Carmichael numbers.

(define (even? x) (= (remainder x 2) 0))
(define (expmod base exp m)
    (cond ((= exp 0) 1)
          ((even? exp)
           (remainder
            (square (expmod base (/ exp 2) m))
             m)) 
          (else
            (remainder
             (* base (expmod base (- exp 1) m))
             m))))

(define (fermat-test-all n)
    (define (fermat-test-all-iter a)
        (if (= a n) #t
            (if (= (expmod a n n) a) (fermat-test-all-iter (+ a 1)) #f)))
    (fermat-test-all-iter 1)
)

; Testing with actual prime numbers
(fermat-test-all 2) ;Value: #t
(fermat-test-all 17) ;Value: #t

; Testing with Carmichael numbers (should be false, but returns true)
(fermat-test-all 561)
(fermat-test-all 1105)
(fermat-test-all 1729)
(fermat-test-all 2465)
(fermat-test-all 2821)
(fermat-test-all 6601)
