; Install `=zero?` for polynomials in the generic arithmetic package. This will allow
; `adjoin-term` to work for polynomials with coefficients that are themselves 
; polynomials.

(load "packages/general.scm")
(load "packages/polynomial.scm")
(load "arithmetic-package.scm")

; Answer

; Inside the `polynomial` package, write the following snippet
; (already included inside `packages/polynomial.scm`)

(define (poly-zero? term-list)
    (if (empty-termlist? term-list)
        #t
        (and (=zero? (first-term term-list))
             (poly-zero? (rest-term term-list)))))

(put '=zero? '(polynomial) (lambda (p) (poly-zero? p)))

; Testing
(install-polynomial-package)

(define p1 
    (make-polynomial 
        'x
        (list (make-term (make-scheme-number 2) (make-scheme-number 0))
              (make-term (make-scheme-number 1) (make-scheme-number 0))
              (make-term (make-scheme-number 0) (make-scheme-number 0))))) ; 0x^2 + 1x + 0

(=zero? p1) ;Value: #t

(define p2
    (make-polynomial
        'x
        (list (make-term (make-scheme-number 2) (make-scheme-number 0))
              (make-term (make-scheme-number 1) (make-scheme-number 2))
              (make-term (make-scheme-number 0) (make-scheme-number 0)))))
(=zero? p2) ;Value: #f