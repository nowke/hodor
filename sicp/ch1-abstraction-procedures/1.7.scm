; The good-enough? test used in computing square roots 
; will not be very effective for finding the square 
; roots of very small numbers. Also, in real computers, 
; arithmetic operations are almost always performed with 
; limited precision. This makes our test inadequate for very 
; large numbers. Explain these statements, with examples showing 
; how the test fails for small and large numbers. An alternative 
; strategy for implementing `good-enough?` is to watch how guess 
; changes from one iteration to the next and to stop when the 
; change is a very small fraction of the guess. Design a square-root 
; procedure that uses this kind of end test. Does this work better 
; for small and large numbers?

; 
; Let's define the previously mentioned square root functions for
; testing small numbers
(define (average x y) (/ (+ x y) 2))
(define (square x) (* x x))

(define (improve guess x) (average guess (/ x guess)))
(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001)
)

(define (sqrt-iter guess x) 
  (if (good-enough? guess x)
    guess
    (sqrt-iter (improve guess x) x)
  )
)

(define (sqrt x)
  (sqrt-iter 1.0 x)
)
;  Let's try some small numbers
(sqrt 0.004) ; res = .06548128198973399, actual = 0.0632455532
(sqrt 0.0006) ; res = 3.7397194007827136e-2, actual = 2.449489743e-2
; As we can observe, our `good-enough?` test fails as the number becomes smaller

; Alternative `good-enough?` function can be,
; (latestGuess - previousGuess)/previousGuess is less than 0.001 (fraction change)
(define (good-enough-alt? guess x)
    (< (abs (/ 
             (- (improve guess x) guess) 
             guess)) 0.001)
)