; Several of the numerical methods described in this chapter 
; are instances of an extremely general computational strategy 
; known as iterative improvement. Iterative improvement says 
; that, to compute something, we start with an initial guess 
; for the answer, test if the guess is good enough, and otherwise 
; improve the guess and continue the process using the improved 
; guess as the new guess. Write a procedure iterative-improve 
; that takes two procedures as arguments: a method for telling
; whether a guess is good enough and a method for improving a 
; guess. iterative-improve should return as its value a procedure 
; that takes a guess as argument and keeps improving the guess 
; until it is good enough. Rewrite the sqrt procedure of Section 1.1.7 
; and the fixed-point procedure of Section 1.3.3 in terms of 
; iterative-improve.

(define (iterative-improve good-enough? improve)
    (lambda (guess) 
        (if (good-enough? guess) guess
        ((iterative-improve good-enough? improve) (improve guess)))))

; (i) Re-write sqrt
(define (sqrt-iter x)
    (define (avg a b) (/ (+ a b) 2))
    (define (good-enough? guess)
        (< (abs (- (square guess) x)) 0.001))
    (define (improve guess)
        (avg guess (/ x guess)))
    ((iterative-improve good-enough? improve) 1.0))

(sqrt-iter 69) ;Value: 8.306626993523448

; (ii) Re-write `fixed-point`
(define (fixed-point f first-guess)
    (define (close-enough? v1 v2)
        (< (abs (- v1 v2)) 0.00001))
    ((iterative-improve
        (lambda (x) (close-enough? x (f x)))
        f) first-guess))

(fixed-point 
    (lambda (x) (+ 1 (/ 1 x))) 
    1.0) ;Value: 1.6180327868852458 (~= φ, golden ratio)