; Suppose you are designing a generic arithmetic system for dealing with the tower 
; of types shown in Figure 2.25: integer, rational, real, complex. For each type 
; (except complex), design a procedure that raises objects of that type one level 
; in the tower. Show how to install a generic raise operation that will work for 
; each type (except complex).

; Source: [https://wizardbook.wordpress.com/2010/12/08/exercise-2-83/](https://wizardbook.wordpress.com/2010/12/08/exercise-2-83/)

(define (raise x)
    (apply-generic 'raise x))

; Integer package
(put 'raise '(integer) (lambda (n) (make-rational n 1)))

; Rational package
(define (rational->real r) 
    (make-real (/ (numer r) (denom r)))) 
(put 'raise '(rational) rational->real)

; Real package
(define (real->complex r)
    (make-complex-from-real-imag r 0))
(put 'raise '(real) real->complex)

