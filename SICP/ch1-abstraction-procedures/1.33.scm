; You can obtain an even more general version of 
; accumulate (Exercise 1.32) by introducing the notion 
; of a filter on the terms to be combined. That is, 
; combine only those terms derived from values in the 
; range that satisfy a specified condition. The resulting 
; `filtered-accumulate` abstraction takes the same 
; arguments as `accumulate`, together with an additional 
; predicate of one argument that specifies the filter. 
; Write `filtered-accumulate` as a procedure. Show how 
; to express the following using filtered-accumulate:
; (a) the sum of the squares of the prime numbers in the 
;     interval a to b (assuming that you have a prime? 
;     predicate already written)

; (b) the product of all the positive integers less than n 
;     are relatively prime to n (i.e., all positive integers 
;     i < n such that GCD(i,n) = 1).

; -----------------------------------
; Answer
; -----------------------------------
; Let's define `filtered-accumulate` function

(define (filtered-accumulate combiner null-value filter term a next b)
    (if (> a b) null-value
        (if (filter a)
            (combiner (term a) 
                      (filtered-accumulate combiner null-value filter term (next a) next b))
            (combiner null-value 
                      (filtered-accumulate combiner null-value filter term (next a) next b)))))

; (a) Let's test with `prime?` procedure
(define (prime? n)
    (define (divides? a b) (= (remainder b a) 0))
    (define (smallest-divisor n) (find-divisor n 2))
    (define (find-divisor n test-divisor)
        (cond ((> (square test-divisor) n) n)
              ((divides? test-divisor n) test-divisor)
              (else (find-divisor n (+ test-divisor 1)))))
    (if (= n 1) #f
        (= n (smallest-divisor n)))
)
(define (inc x) (+ x 1))
(define (sum-of-squares-prime a b)
    (filtered-accumulate + 0 prime? square a inc b))

(sum-of-squares-prime 1 10) ;Value: 87 (2^2 + 3^2 + 5^2 + 7^2)

; (b) Define `relatively-prime?` filter
(define (gcd a b) 
    (cond ((< a b) (gcd b a)) 
          ((= b 0) a) 
          (else (gcd b (remainder a b))))) 
   
(define (relatively-prime? a b)
    (= (gcd a b) 1))
(define (product-of-relative-prime-nums n)
    (define (filter x) (relatively-prime? x n))
    (define (f x) x)
    (filtered-accumulate * 1 filter f 1 inc n)
)
(product-of-relative-prime-nums 10) ;Value: 189