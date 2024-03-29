; The sine of an angle (specified in radians) can be 
; computed by making use of the approximation sin x ≈ x 
; if x is sufficiently small, and the trigonometric identity
; sin x = 3 sin (x/3) − 4 sin^3 (x/3)
; to reduce the size of the argument of sin. 
; (For purposes of this exercise an angle is considered 
; “sufficiently small” if its magnitude is not greater than 0.1 
; radians.) These ideas are incorporated in the following procedures:
(define (cube x) (* x x x))
(define (p x) (- (* 3 x) (* 4 (cube x))))

(define (sine angle)
    (if (not (> (abs angle) 0.1)) 
        angle
        (p (sine (/ angle 3.0)))))

; a) How many times is the procedure "p" applied when (sine 12.15) is evaluated?
; Answer
; (sine 12.15)
; (p (sine 4.05))
; (p (p (sine 1.35)))
; (p (p (p (sine 0.45))))
; (p (p (p (p (sine 0.15)))))
; (p (p (p (p (p (sine 0.05))))))
; (p (p (p (p (p 0.05)))))
; => Procedure "p" is applied 5 times

; b) What is the order of growth in space and number of steps 
;    (as a function of a) used by the process generated by the 
;    sine procedure when (sine a) is evaluated?
; Anwer
; => Order of growth = O(log (a))
; 
; To calculate number of steps,
;   0.05 x 3 x 3 x 3 x 3 x 3 = 12.15
;   => 0.05 x 3^5 = 12.15
;   => 3^5 = (12.15 / 0.05)
;   -> rewriting, 5 = log3(12.15 / 0.05)  (log base 3)
;   Hence, we got 5 (number of steps) like above
;   In general, we can say that,
; =>  no.of.steps = log3(a / 0.1) (since at 0.1, applying p is stopped)

;   Since this number is not an integer, we would have to take upper
;   bound of the number (ceiling)
;  
;   i.e. No.of.steps = ceil(log (a / 0.1) / log 3)  (since, logb(a) = log a / log b)
(define (no-of-steps a) 
    (ceiling (/ (log (/ a 0.1)) (log 3)))    
)