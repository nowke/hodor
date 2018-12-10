; Alyssa P. Hacker doesn’t see why if needs to be 
; provided as a special form. “Why can’t I just 
; define it as an ordinary procedure in terms of cond?” 
; she asks. Alyssa’s friend Eva Lu Ator claims this 
; can indeed be done, and she defines a new version of if:
(define (new-if predicate then-clause else-clause) 
    (cond (predicate then-clause)
        (else else-clause)
    )
)
(new-if (= 2 3) 0 5) ; 5
(new-if (= 1 1) 0 5) ; 0
; Delighted, Alyssa uses new-if to rewrite the 
; square-root program:
(define (average x y) (/ (+ x y) 2))
(define (square x) (* x x))

(define (improve guess x) (average guess (/ x guess)))
(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001)
)

(define (sqrt-iter guess x) 
  (new-if (good-enough? guess x)
    guess
    (sqrt-iter (improve guess x) x)
  )
)

(define (sqrt x)
  (sqrt-iter 1.0 x)
)
; What happens when Alyssa attempts to use this 
; to compute square roots? Explain

; Answer:
; Since the condition is inside a "function" `new-if`,
; inside `sqrt-iter`, the "else-clause" will be evaluated.
; This will lead to infinite recursion scenario.
; Note: This will continue until stack overflow