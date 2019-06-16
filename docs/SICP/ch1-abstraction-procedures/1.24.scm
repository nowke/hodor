; Modify the timed-prime-test procedure of Exercise 1.22 
; to use fast-prime? (the Fermat method), and test each 
; of the 12 primes you found in that exercise. Since the 
; Fermat test has Θ(log n) growth, how would you expect 
; the time to test primes near 1,000,000 to compare with 
; the time needed to test primes near 1000? Do your data 
; bear this out? Can you explain any discrepancy you find?

; Let's define fast-prime procedure
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
(define (fermat-test n) 
    (define (try-it a)
     (= (expmod a n n) a))
    (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times) 
    (cond ((= times 0) true)
          ((fermat-test n) (fast-prime? n (- times 1))) 
          (else false)))
(define (prime? n)
   (fast-prime? n 500)) 

; Define `timed-prime-test` procedure
(define (timed-prime-test n)
    (newline)
    (display n)
    (start-prime-test n (runtime)))
(define (start-prime-test n start-time)
    (if (prime? n)
        (report-prime (- (runtime) start-time)))) 
(define (report-prime elapsed-time)
    (display " *** ")
    (display elapsed-time))

; Test the timing on large prime numbers
(timed-prime-test 1000000007)    ; .05000000000000002
(timed-prime-test 1000000009)    ; .03999999999999998
(timed-prime-test 1000000021)    ; .04000000000000001
(timed-prime-test 10000000019)   ; .06
(timed-prime-test 10000000033)   ; .04999999999999999
(timed-prime-test 10000000061)   ; .06
(timed-prime-test 100000000003)  ; .06
(timed-prime-test 100000000019)  ; .06000000000000005
(timed-prime-test 100000000057)  ; .06000000000000005
(timed-prime-test 1000000000039) ; .06999999999999995
(timed-prime-test 1000000000061) ; .06000000000000005

; Test by doubling number of digits
(timed-prime-test 1000003)       ; 3.9999999999999925e-2
(timed-prime-test 1000000000063) ; .08000000000000007
; i.e.
; 10^6 => 0.04
; 10^12 => 0.08, which is a logarithmic scale