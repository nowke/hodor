; Define a generic equality predicate `equ?` that tests the equality of two numbers, 
; and install it in the generic arithmetic package. This operation should work for 
; ordinary numbers, rational numbers, and complex numbers.

(load "arithmetic-package.scm")

; (define (numer x) (apply-generic 'numer x))
; (define (denom x) (apply-generic 'denom x))

(define (equ? x y) (apply-generic 'equ? x y))

; Define `equ?` for each type
(put 'equ? '(scheme-number scheme-number) =) ; Equality operator for number
(put 'equ? '(rational_ rational_)   ; => Defined inside arithmetic-package itself
     (lambda (x y) 
        (and (= (numer x) (numer y))
             (= (denom x) (denom y)))))
(put 'equ? '(complex complex)
     (lambda (z1 z2)
        (and (= (real-part z1) (real-part z2))
             (= (imag-part z1) (imag-part z2)))))

; Testing
(equ? (make-scheme-number 42) (make-scheme-number 42)) ;#t
(equ? (make-scheme-number 42) (make-scheme-number 0))  ;#f
(equ? (make-rational 5 10) (make-rational 100 200)) ;#t
(equ? (make-complex-from-real-imag 1 2) (make-complex-from-real-imag 1 2)) ;#t