; We saw in Section 1.3.3 that attempting to compute square 
; roots by naively finding a fixed point of y 􏰕→ x/y does not 
; converge, and that this can be fixed by average damping. 
; The same method works for finding cube roots as fixed 
; points of the average-damped y 􏰕→ x/y2. Unfortunately, the 
; process does not work for fourth roots — a single average 
; damp is not enough to make a fixed-point search for y 􏰕→ x/y3 
; converge. On the other hand, if we average damp twice (i.e., 
; use the average damp of the average damp of y 􏰕→ x/y3) the 
; fixed-point search does converge. Do some experiments to 
; determine how many average damps are required to compute 
; nth roots as a fixed-point search based upon repeated average 
; damping of y 􏰕→ x/yn−1. Use this to implement a simple procedure 
; for computing nth roots using fixed-point, average-damp, and 
; the repeated procedure of Exercise 1.43. Assume that any 
; arithmetic operations you need are available as primitives.

(define (average-damp f)
    (lambda (x) ((lambda (a b) (/ (+ a b) 2)) x (f x))))
(define (fixed-point f first-guess)
    (define (close-enough? v1 v2)
        (< (abs (- v1 v2)) 0.00001))
    (define (try guess)
        (let ((next (f guess)))
            (if (close-enough? guess next)
                next
                (try next))))
    (try first-guess))
(define (compose f g)
    (lambda (x) (f (g x))))
(define (repeated f n)
    (if (= n 1) (lambda (x) (f x))
        (compose f (repeated f (- n 1))))
)

; Let's write nth-root procedure
(define (nth-root x n damps)
    (fixed-point 
        ((repeated average-damp damps)
            (lambda (y) (/ x (expt y (- n 1)))))
        1.0))

; Let's try with some values
(nth-root 1296 4 3) ;Value: 6.000005021306138 (6^4 = 1296)
(nth-root 2197.5 5 3) ;Value: 4.660003848858503 (4.66^5 ~= 2197.5)

; It is sufficient to apply number of damps log2(n) times
(define (nth-root x n)
    (define damps (floor (/ (log n) (log 2))))
    (fixed-point 
        ((repeated average-damp damps)
            (lambda (y) (/ x (expt y (- n 1)))))
        1.0))

(nth-root 1048576 20) ;Value: 1.999999063225966