; Derivate of a function
(define (deriv g)
    (lambda (x) (/ (- (g (+ x dx)) (g x)) dx)))
(define dx 0.00001)

; Evaluate derivative of x^3 -> 3x^2
; i.e. x -> x^3 at x=5 is 75
(define (cube x) (* x x x))
((deriv cube) 5) ;Value: 75.00014999664018

; Newton's method as fixed point process,
; f(x) = x - g(x)/Dg(x)
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
(define (newton-transform g)
    (lambda (x) (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess) (fixed-point (newton-transform g) guess))

; Finding square root
; y -> y^2 - x 
(define (newton-sqrt x)
    (newtons-method (lambda (y) (- (square y) x)) 1.0))

(newton-sqrt 26) ;Value: 5.099019513592785

; Abstractions and first-class procedures
(define (fixed-point-of-transform g transform guess) 
    (fixed-point (transform g) guess))

; Square root v1 (using average damp y->x/y)
(define (average-damp f)
    (lambda (x) ((lambda (a b) (/ (+ a b) 2)) x (f x))))
(define (sqrt-v1 x)
    (fixed-point-of-transform
        (lambda (y) (/ x y)) average-damp 1.0))
(sqrt-v1 28) ;Value: 5.291502622129181

; Square root v2 (using Newton's transform)
(define (sqrt-v2 x) (fixed-point-of-transform
            (lambda (y) (- (square y) x)) newton-transform 1.0))
(sqrt-v2 28) ;Value: 5.291502622129181