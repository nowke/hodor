; Straightforward primality testing
; Test divisors b/w 1 to sqrt(n)
; Order of growth = Θ(√n).
(define (divides? a b) (= (remainder b a) 0))

(define (prime? n)
    (define (smallest-divisor n) (find-divisor n 2))
    (define (find-divisor n test-divisor)
        (cond ((> (square test-divisor) n) n)
              ((divides? test-divisor n) test-divisor)
              (else (find-divisor n (+ test-divisor 1)))))
    (= n (smallest-divisor n))
)
(prime? 11)
(prime? 19)
(prime? 20)

; Fermat test
; If n is a prime number and a is any positive integer less 
; than n, then a raised to the nth power is congruent to a modulo n
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
(fast-prime? 19 10)