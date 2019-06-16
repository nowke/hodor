; After debugging her program, Alyssa shows it to a potential 
; user, who complains that her program solves the wrong problem. 
; He wants a program that can deal with numbers represented as 
; a center value and an additive tolerance; for example, he 
; wants to work with intervals such as 3.5 ± 0.15 rather than 
; [3.35, 3.65]. Alyssa returns to her desk and fixes this problem 
; by supplying an alternate constructor and alternate selectors:
(define (make-center-width c w) 
    (make-interval (- c w) (+ c w)))
(define (center i)
    (/ (+ (lower-bound i) (upper-bound i)) 2))
(define (width i)
    (/ (- (upper-bound i) (lower-bound i)) 2))
; Unfortunately, most of Alyssa’s users are engineers. Real engineering 
; situations usually involve measurements with only a small uncertainty, 
; measured as the ratio of the width of the interval to the midpoint of 
; the interval. Engineers usually specify percentage tolerances on the 
; parameters of devices, as in the resistor specifications given earlier.
; Define a constructor `make-center-percent` that takes a center and a 
; percentage tolerance and produces the desired interval. You must also 
; define a selector percent that produces the percentage tolerance for 
; a given interval. The center selector is the same as the one shown above.

; Answer
; --------------------------------
; Let's define necessary helpers
(define (make-interval a b)
    (cons (min a b) (max a b)))
(define (lower-bound z) (car z))
(define (upper-bound z) (cdr z))

; Let's construct `make-center-percent` procedure
(define (make-center-percent c p)
    (make-center-width c (/ (* c p) 100)))

; `percent` constructor can be written as,
(define (percent i)
    (* (/ (width i) (center i)) 100.0))

; Testing
(define resistor1 (make-center-percent 5.6 10))
(percent resistor1) ;Value: 10.000000000000009
(center resistor1)  ;Value: 5.6
