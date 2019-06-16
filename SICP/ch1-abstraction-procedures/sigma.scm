; Arriving at higher abstraction notation "Sigma"

; (1) Compute sum from a to b
;    a + (a + 1) + ... + (b - 1) + b
(define (sum-integers a b) (if (> a b)
      0
      (+ a (sum-integers (+ a 1) b))))
(sum-integers 1 10) ;Value: 55

; (2) Sum of cubes of integer from a to b
;    a^3 + (a+1)^3 + ... + (b-1)^3 + b^3
(define (cube x) (* x x x))
(define (sum-cubes a b) 
    (if (> a b)
      0
      (+ (cube a)
         (sum-cubes (+ a 1) b))))
(sum-cubes 2 4) ;Value: 99

; (3) Sum of the following series (Pi-sum)
;  (1/ (1 * 3)) + (1 / (5 * 7)) + (1 / (9 * 11)) + ... = π/8
(define (pi-sum a b)
    (if (> a b)
        0
        (+ (/ 1.0 (* a (+ a 2)))
           (pi-sum (+ a 4) b))))
(* 8 (pi-sum 1 1000)) ;Value: 3.139592655589783

; Abstracting the common pattern,
(define (sigma term a next b)
    (if (> a b) 
        0
        (+ (term a)
           (sigma term (next a) next b))))

; Problem (2) can be written as,
(define (inc n) (+ n 1))
(define (sum-cubes a b)
    (sigma cube a inc b))
(sum-cubes 2 4) ;Value: 99

; Problem (3) can be written as,
(define (pi-sum a b) 
    (define (pi-term x)
        (/ 1.0 (* x (+ x 2)))) 
    (define (pi-next x)
        (+ x 4))
  (sigma pi-term a pi-next b))
(* 8 (pi-sum 1 1000)) ;Value: 3.139592655589783