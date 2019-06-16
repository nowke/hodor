; The following procedure computes a mathematical 
; function called "Ackermann’s function"
(define (A x y) 
    (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1) (A x (- y 1))))))

; Q1) What are the values of the following expressions?
(A 1 10)
(A 2 4)
(A 3 3)
; A1) 
; (A 1 10) 
;   => (A 0 (A 1 9))
;   => (A 0 (A 0 (A 1 8)))
;   => ...
;   => 1024
; (A 2 4)
;   => 65536
; (A 3 3)
;   => 65536
;
; Consider the following procedures, where A 
; is the procedure defined above:
(define (f n) (A 0 n)) 
(define (g n) (A 1 n))
(define (h n) (A 2 n))
(define (k n) (* 5 n n))
; Q2) Give concise mathematical definitions for the 
;     functions computed by the procedures f, g, and h 
;     for positive integer values of n. 
;     For example, (k n) computes 5n^2
;
; A2) 
;  f(n) -> A(0, n)
;       -> x=0 -> 2y -> 2n
;  => f(n) = 2n

;  g(n) -> A(1, n)
;       -> A(0, A(1, n-1))
;       -> 2 * A(1, n-1)
;       -> 2 * A(0, A(1, n-2))
;       -> 2 * 2 * A(0, A(1, n-3))
;       -> 2 * 2 * 2 ... * 2 (n times)
;  => g(n) = 2^n
;
;  h(n) -> A(2, n)
;       -> A(1, A(2, n-1))
;       -> 2 ^ A(2, n -1)
;       -> 2 ^ ( A(1, A(2, n - 2)) )
;       -> 2 ^ 2 ^ A(2, n - 2)
;  => h(n) = 2 ^ 2 ^ 2 .... ^ 2 (n times)
