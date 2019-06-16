; A univariate polynomial can be divided by another one to produce a polynomial 
; quotient and a polynomial remainder. For example,
;
; $\frac{x^5 - 1}{x^2 - 1} = x^3 + x$, remainder $x-1$
;
; Division can be performed via long division. That is, divide the highest-order 
; term of the dividend by the highest-order term of the divisor. The result is the 
; first term of the quotient. Next, multiply the result by the divisor, subtract 
; that from the dividend, and produce the rest of the answer by recursively dividing
; the difference by the divisor. Stop when the order of the divisor exceeds the order
; of the dividend and declare the dividend to be the remainder. Also, if the dividend
; ever becomes zero, return zero as both quotient and remainder.
;
; We can design a div-poly procedure on the model of add- poly and mul-poly. The 
; procedure checks to see if the two polys have the same variable. If so, div-poly 
; strips off the variable and passes the problem to div-terms, which performs the 
; division operation on term lists. div-poly finally reattaches the variable to the 
; result supplied by div-terms. It is convenient to design div-terms to compute both
; the quotient and the remainder of a division. div-terms can take two term lists as
; arguments and return a list of the quotient term list and the remainder term list.
;
; Complete the following definition of div-terms by filling in the missing expressions. 
; Use this to implement div-poly, which takes two polys as arguments and returns a 
; list of the quotient and remainder polys.

; Add the following to `polynomial` package
(define (div-terms L1 L2) 
    (if (empty-termlist? L1)
        (list (the-empty-termlist) (the-empty-termlist)) 
        (let ((t1 (first-term L1))
              (t2 (first-term L2)))
            (if (> (order t2) (order t1))
                (list (the-empty-termlist) L1)
                (let ((new-c (div (coeff t1) (coeff t2)))
                      (new-o (- (order t1) (order t2)))) 
                    (let ((rest-of-result
                          (div-terms
                            (add-terms
                                L1
                                (negate-terms
                                    (mul-term-by-all-terms
                                        (make-term new-o new-c)
                                        L2)))
                            L2))) 
                        (cons
                            (adjoin-term
                                (make-term new-o new-c)
                                (car rest-of-result))
                            (cdr rest-of-result))))))))

; Redefining the polynomial package
(define (install-polynomial-package)
    ;; internal procedures
    ;; representation of poly
    (define (make-poly variable term-list) (cons variable term-list))
    (define (variable p) (car p))
    (define (term-list p) (cdr p))
    ; ⟨procedures same-variable? and variable? from section 2.3.2⟩
    (define (variable? x) (symbol? x))
    (define (same-variable? v1 v2)
        (and (variable? v1) (variable? v2) (eq? v1 v2)))

    ;; representation of terms and term lists
    ;; ⟨procedures adjoin-term . . . coeff from text below⟩

    ;; Add
    (define (add-poly p1 p2)
        (if (same-variable? (variable p1) (variable p2))
            (make-poly 
                (variable p1)
                (add-terms (term-list p1) (term-list p2)))
            (error "Polys not in same var: ADD-POLY" (list p1 p2))))
    (define (add-terms L1 L2)
        (cond ((empty-termlist? L1) L2)
              ((empty-termlist? L2) L1)
              (else
                (let ((t1 (first-term L1))
                      (t2 (first-term L2)))
                    (cond ((> (order t1) (order t2))
                           (adjoin-term
                            t1
                            (add-terms (rest-terms L1) L2)))
                          ((< (order t1) (order t2))
                           (adjoin-term
                            t2
                            (add-terms L1 (rest-terms L2))))
                          (else
                            (adjoin-term
                                (make-term
                                    (order t1)
                                    (add (coeff t1) (coeff t2)))
                                (add-terms 
                                    (rest-terms L1)
                                    (rest-terms L2)))))))))

    ;; Multiply
    (define (mul-poly p1 p2)
        (if (same-variable? (variable p1) (variable p2))
            (make-poly 
                (variable p1)
                (mul-terms (term-list p1) (term-list p2)))
            (error "Polys not in same var: MUL-POLY" (list p1 p2))))
    (define (mul-terms L1 L2)
        (if (empty-termlist? L1)
            (the-empty-termlist)
            (add-terms
                (mul-term-by-all-terms (first-term L1) L2)
                (mul-terms (rest-terms) L2))))
    (define (mul-term-by-all-terms t1 L)
        (if (empty-termlist? L)
            (the-empty-termlist)
            (let ((t2 (first-term L)))
                (adjoin-term
                    (make-term
                        (+ (order t1) (order t2))
                        (mul (coeff t1) (coeff t2)))
                    (mul-term-by-all-terms
                        t1
                        (rest-terms L))))))
    
    (define (poly-zero? term-list)
        (if (empty-termlist? term-list)
            #t
            (and (=zero? (coeff (first-term term-list)))
                 (poly-zero? (rest-terms term-list)))))
    ;; Subtraction
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

    ;; Division
    (define (div-terms L1 L2) 
        (if (empty-termlist? L1)
            (list (the-empty-termlist) (the-empty-termlist)) 
            (let ((t1 (first-term L1))
                  (t2 (first-term L2)))
                (if (> (order t2) (order t1))
                    (list (the-empty-termlist) L1)
                    (let ((new-c (div (coeff t1) (coeff t2)))
                          (new-o (- (order t1) (order t2)))) 
                        (let ((rest-of-result
                              (div-terms
                                (add-terms
                                    L1
                                    (negate-terms 
                                        (mul-term-by-all-terms
                                            (make-term new-o new-c)
                                            L2)))
                                L2))) 
                            (list
                                (adjoin-term
                                    (make-term new-o new-c)
                                    (car rest-of-result))
                                (cadr rest-of-result))))))))
    (define (div-poly p1 p2)
        (if (same-variable? (variable p1) (variable p2))
            (let ((ans (div-terms (term-list p1)
                                  (term-list p2))))
                (list (make-poly (variable p1)
                                 (car ans))
                      (make-poly (variable p1)
                                 (cadr ans))))
            (error "Polys not in same var: DIV-POLY" (list p1 p2))))

    ;; interface to rest of the system
    (define (tag p) (attach-tag 'polynomial p))
    (put '=zero? '(polynomial) (lambda (p) (poly-zero? (term-list p))))
    (put 'add '(polynomial polynomial)
        (lambda (p1 p2) (tag (add-poly p1 p2))))
    (put 'mul '(polynomial polynomial)
        (lambda (p1 p2) (tag (mul-poly p1 p2))))
    (put 'div '(polynomial polynomial)
        (lambda (p1 p2) (tag (div-poly p1 p2))))
    (put 'make 'polynomial
        (lambda (var terms) (tag (make-poly var terms))))
    (put 'negate '(polynomial)
        (lambda (p) 
            (make-polynomial 
                (variable p)
                (negate-terms (term-list p)))))
    (put 'sub '(polynomial polynomial)
        (lambda (p1 p2) (tag (sub-poly p1 p2))))
    'done)

;; representation of terms
(define (adjoin-term term term-list)
    (if (=zero? (coeff term))
        term-list
        (cons term term-list)))
(define (the-empty-termlist) '())
(define (first-term term-list) (car term-list))
(define (rest-terms term-list) (cdr term-list))
(define (empty-termlist? term-list) (null? term-list))
(define (make-term order coeff) (list order coeff))
(define (order term) (car term))
(define (coeff term) (cadr term))

(define (make-polynomial var terms)
    ((get 'make 'polynomial) var terms))

; Testing
(load "arithmetic-package.scm")

(install-polynomial-package)

(define p1 
    (make-polynomial 
        'x
        (list (make-term  5  (make-scheme-number 1))
              (make-term  0  (make-scheme-number -1))))) ; x^5 - 1

(define p2 
    (make-polynomial 
        'x
        (list (make-term  2  (make-scheme-number 1))
              (make-term  0  (make-scheme-number -1))))) ; x^2 - 1

(div p1 p2)
; Note: This returns an error, couldn't get this working :/