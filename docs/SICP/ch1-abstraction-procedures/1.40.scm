; Define a procedure cubic that can be used together with the 
; `newtons-method` procedure in expressions of the form
; (newtons-method (cubic a b c) 1)
; to approximate zeros of the cubic x3 + ax2 + bx + c.
(define (cube x) (* x x x))
(define (cubic a b c)
    (lambda (x) (+ (cube x) (* a (square x)) (* b x) c)))

; Using with `newtons-method`
(define (deriv g)
    (lambda (x) (/ (- (g (+ x dx)) (g x)) dx)))
(define dx 0.00001)
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
(newtons-method (cubic 3 1 19) 1) ;Value: -3.959409544418823 (~4)