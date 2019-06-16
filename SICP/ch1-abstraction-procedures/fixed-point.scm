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

(fixed-point cos 1.0) ;Value: .7390822985224023 (cos (value) = value)
(fixed-point (lambda (y) (+ (sin y) (cos y))) 
             1.0) ;Value: 1.2587315962971173
