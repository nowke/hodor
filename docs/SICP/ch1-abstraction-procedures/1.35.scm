; Show that the golden ratio φ (Section 1.2.2) is a 
; fixed point of the transformation x 􏰕→ 1 + 1/x, and 
; use this fact to compute φ by means of the fixed-point 
; procedure.

(define tolerance 0.00001)
(define (fixed-point f first-guess)
    (define (close-enough? v1 v2)
        (< (abs (- v1 v2)) tolerance))
    (define (try guess)
        (let ((next (f guess)))
            (if (close-enough? guess next)
                next
                (try next))))
    (try first-guess))

(fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0) ;Value: 1.6180327868852458

; The result is close, as we know φ=1.61803398875