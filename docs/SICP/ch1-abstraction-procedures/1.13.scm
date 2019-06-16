; Prove that Fib(n) is the closest integer to
; (φ^n)/√5, where φ = (1 + √5)/2 
; Hint: Let ψ = (1 − √5)/2
; Use induction and the definition of the Fibonacci numbers
; to prove that Fib(n) = (φ^n − ψ^n )/√5

(define phi 
    (/ (+ 1 (sqrt 5)) 2))

(define (f n)
    (/ (expt phi n) (sqrt 5)))

(f 0)
(f 1)
(f 2)
(f 3)
(f 4)
(f 5)
(f 6)

; Proof by Induction method: 
; http://www.billthelizard.com/2009/12/sicp-exercise-113-fibonacci-and-golden.html