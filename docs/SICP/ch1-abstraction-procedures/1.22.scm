; Most Lisp implementations include a primitive called 
; `runtime` that returns an integer that specifies the 
; amount of time the system has been running (measured, 
; for example, in microseconds). The following `timed-prime-test` 
; procedure, when called with an integer n, prints n and 
; checks to see if n is prime. If n is prime, the procedure 
; prints three asterisks followed by the amount of time used 
; in performing the test.
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

; Using this procedure, write a procedure `search-for-primes` 
; that checks the primality of consecutive odd integers in a 
; specified range. Use your procedure to find the three 
; smallest primes larger than 1000; larger than 10,000; 
; larger than 100,000; larger than 1,000,000. Note the time 
; needed to test each prime. Since the testing algorithm has 
; order of growth of Θ(√n), you should expect that testing for 
; primes around 10,000 should take about √10 times as long as 
; testing for primes around 1000. Do your timing data bear this out? 
; How well do the data for 100,000 and 1,000,000 support the Θ(√n) 
; prediction? Is your result compatible with the notion that 
; programs on your machine run in time proportional to the number 
; of steps required for the computation?

; ANSWER
; Let's define `prime?` procedure
(define (divides? a b) (= (remainder b a) 0))
(define (prime? n)
    (define (smallest-divisor n) (find-divisor n 2))
    (define (find-divisor n test-divisor)
        (cond ((> (square test-divisor) n) n)
              ((divides? test-divisor n) test-divisor)
              (else (find-divisor n (+ test-divisor 1)))))
    (= n (smallest-divisor n))
)
; Write `search-for-primes` procedure
(define (even? x) (= (remainder x 2) 0))
(define (search-for-primes start end)
    (define (search-for-primes-iter cur)
        (if (<= cur end) (timed-prime-test cur))
        (if (<= cur end) (search-for-primes-iter (+ 2 cur)))
    )
    (search-for-primes-iter (if (even? start) 
                                (+ 1 start)
                                start)))

(search-for-primes 1000 1020)
(search-for-primes 10000 10050)
(search-for-primes 1000000 1000100)

; We almost see zero for computation time. 
; Hence we need to increase the quantity of numbers

(search-for-primes 1000000000 1000000025)         ; 10^9
(search-for-primes 10000000000 10000000080)       ; 10^10
(search-for-primes 100000000000 100000000060)     ; 10^11
(search-for-primes 1000000000000 1000000000065)   ; 10^12
(search-for-primes 10000000000000 10000000000080) ; 10^13

; On my MacBook air, the following times were reported
; 10^9  => ~ 0.06
; 10^10 => ~ 0.16
; 10^11 => ~ 0.5
; 10^12 => ~ 1.45
; 10^13 => ~ 4.75

; √10 ~ 3.16
; If we calculate,
; (a) Time(10^11) / Time(10^10) = 0.5 / 0.16 = 3.125
; (b) Time(10^10) / Time(10^9)  = 0.16 / 0.06 = 2.67
; (c) Time(10^12) / Time(10^11)  = 1.45 / 0.5 = 2.9
; (c) Time(10^13) / Time(10^12)  = 4.75 / 1.45 = 3.275