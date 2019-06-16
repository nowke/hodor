; Define a better version of make-rat that handles both 
; positive and negative arguments. make-rat should normalize 
; the sign so that if the rational number is positive, both 
; the numerator and denominator are positive, and if the 
; rational number is negative, only the numerator is negative.
(define (numer x) (car x))
(define (denom x) (cdr x))
(define (print-rat x)
    (newline)
    (display (numer x))
    (display "/")
    (display (denom x)))

(define (make-rat n d)
    (define g (gcd n d))
    (let ((n-pos (abs (/ n g)))
          (d-pos (abs (/ d g))))
         (cond ((> (* n d) 0) (cons n-pos d-pos))
               ((< (* n d) 0) (cons (- n-pos) d-pos)))))

(print-rat (make-rat 6 18)) ;1/3
(print-rat (make-rat -6 -18)) ;1/3
(print-rat (make-rat 6 -18)) ;-1/3
(print-rat (make-rat -6 18)) ;-1/3