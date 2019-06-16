; The `smallest-divisor` procedure shown at the start 
; of this section does lots of needless testing: After 
; it checks to see if the number is divisible by 2 there 
; is no point in checking to see if it is divisible by any 
; larger even numbbers. This suggests that the values 
; used for test-divisor should not be 2, 3, 4, 5, 6, . . ., 
; but rather 2, 3, 5, 7, 9, . . ..
; To implement this change, define a procedure next that 
; returns 3 if its input is equal to 2 and otherwise returns 
; its input plus 2. Modify the smallest-divisor procedure 
; to use (next test-divisor) instead of (+ test-divisor 1). 
; With timed-prime-test incorporating this modified version 
; of smallest-divisor, run the test for each of the 12 primes 
; found in Exercise 1.22. Since this modification halves the 
; number of test steps, you should expect it to run about twice as fast. 
; Is this expectation confirmed? If not, what is the observed ratio
; of the speeds of the two algorithms, and how do you explain the 
; fact that it is different from 2?
(define (divides? a b) (= (remainder b a) 0))
(define (prime? n)
    (define (smallest-divisor n) (find-divisor n 2))
    (define (find-divisor n test-divisor)
        (cond ((> (square test-divisor) n) n)
              ((divides? test-divisor n) test-divisor)
              (else (find-divisor n (next test-divisor)))))
    (= n (smallest-divisor n))
)
(define (next n)
  (if (= n 2)
      3
      (+ n 2)))
; Timed prime test
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

; 12 primes found in `Exercise 1.22`
(timed-prime-test 1000000007)    ; .03
(timed-prime-test 1000000009)    ; .03
(timed-prime-test 1000000021)    ; .04000000000000001
(timed-prime-test 10000000019)   ; .08999999999999997
(timed-prime-test 10000000033)   ; .09000000000000002
(timed-prime-test 10000000061)   ; .08999999999999997
(timed-prime-test 100000000003)  ; .27 
(timed-prime-test 100000000019)  ; .30000000000000004
(timed-prime-test 100000000057)  ; .28
(timed-prime-test 1000000000039) ; 1.0399999999999998
(timed-prime-test 1000000000061) ; .8500000000000001
(timed-prime-test 1000000000063) ; .8600000000000003

; Comparing the order v/s time from previous and current
; implementations
; ------------------------------------------------
; | Order  | Previous | Current | Factor reduced |
; |----------------------------------------------|
; |  10^9  |   0.06   |  0.04   |    1.5         |
; |  10^10 |   0.16   |  0.9    |    1.77        |
; |  10^11 |   0.5    |  0.3    |    1.66        |
; |  10^12 |   1.45   |  0.85   |    1.7         |
; |----------------------------------------------|
;
; As we observe, factor reduced is not exactly 2, but ~1.5-1.6
; This is because, we spend some time during IF condition
; present in `next` procedure