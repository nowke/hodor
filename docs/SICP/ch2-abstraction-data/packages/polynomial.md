
# polynomial

```scheme
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

    ;; interface to rest of the system
    (define (tag p) (attach-tag 'polynomial p))
    (put '=zero? '(polynomial) (lambda (p) (poly-zero? (term-list p))))
    (put 'add '(polynomial polynomial)
        (lambda (p1 p2) (tag (add-poly p1 p2))))
    (put 'mul '(polynomial polynomial)
        (lambda (p1 p2) (tag (mul-poly p1 p2))))
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
```
