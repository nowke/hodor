; Define a generic predicate `=zero?` that tests if its argument is zero, and 
; install it in the generic arithmetic package. This operation should work for 
; ordinary numbers, rational numbers, and complex numbers.

(load "arithmetic-package.scm")

(define (=zero? x) (apply-generic '=zero? x)) 

; Define `=zero?` in each package
(put '=zero? 'scheme-number (lambda (x) (= x 0)))
(put '=zero? 'rational_ (lambda (x) (= (numer x) 0)))
(put '=zero? 'complex (lambda (z) (= (real-part z) (imag-part z) 0)))

; Testing
(=zero? (make-scheme-number 0)) ;#t
(=zero? (make-rational 0 10)) ;#t
(=zero? (make-rational 1 10)) ;#f
(=zero? (make-complex-from-real-imag 0 0)) ;#t
(=zero? (make-complex-from-real-imag 0 1)) ;#f