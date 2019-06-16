; Extend the polynomial system to include subtraction of polynomials. (Hint: You may
; find it helpful to define a generic negation operation.)

(load "packages/general.scm")

; Add the following genric negation operations
; (i) scheme-numer
(put 'negate '(scheme-number) (lambda (x) (tag (- x))))

; (ii) rational
(put 'negate '(rational) 
    (lambda (r) (make-rat (- numer r) (denom r))))

; (iii) complex
(put 'negate '(complex)
    (lambda (z) (make-from-real-imag 
                    (- (real-part z))
                    (- (imag-part z)))))

; (iv) Polynomial package
(define (negate-terms term-list)
    (if (empty-termlist? term-list)
        the-empty-termlist
        (adjoin-term
            (make-term 
                (order (first-term term-list)) 
                (negate (coeff (first-term term-list))))
            (negate-terms (rest-terms term-list)))))
(define (sub-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly 
            (variable p1)
            (add-terms (term-list p1) (negate-terms (term-list p2))))
        (error "Polys not in same var: SUB-POLY" (list p1 p2))))
(put 'negate '(polynomial)
    (lambda (p) 
        (make-polynomial 
            (variable p)
            (negate-terms (term-list p)))))
(put 'sub '(polynomial polynomial)
    (lambda (p1 p2) (tag (sub-poly p1 p2))))

(load "packages/polynomial.scm")
(load "arithmetic-package.scm")

(install-polynomial-package)

; Testing
(define p1 
    (make-polynomial 
        'x
        (list (make-term  2  (make-scheme-number 2))
              (make-term  1  (make-scheme-number 5))
              (make-term  0  (make-scheme-number 3))))) ; 2x^2 + 5x + 3

(define p2 
    (make-polynomial 
        'x
        (list (make-term  3  (make-scheme-number 1))
              (make-term  1  (make-scheme-number 4))
              (make-term  0  (make-scheme-number 2))))) ; x^3 + 4x + 2

(sub p2 p1)
; (polynomial x 
;    (3 (scheme-number . 1)) 
;    (2 (scheme-number . -2))
;    (1 (scheme-number . -1))
;    (0 (scheme-number . -1)) -> x^3 - 2x^2 - c - 1