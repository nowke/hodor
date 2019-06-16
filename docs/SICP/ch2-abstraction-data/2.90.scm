; Suppose we want to have a polynomial system that is efficient for both sparse and 
; dense polynomials. One way to do this is to allow both kinds of term-list 
; representations in our system. The situation is analogous to the complex-number 
; example of Section 2.4, where we allowed both rectangular and polar representations. 
; To do this we must distinguish different types of term lists and make the 
; operations on term lists generic. Redesign the polynomial system to implement this 
; generalization. This is a major effort, not a local change.

; Dense package
(define (install-polynomial-package-dense)
    ;; internal procedures
    ;; representation of poly
    (define (make-poly variable term-list) (cons variable term-list))
    (define (variable p) (car p))
    (define (term-list p) (cdr p))
    ; ⟨procedures same-variable? and variable? from section 2.3.2⟩
    (define (variable? x) (symbol? x))
    (define (same-variable? v1 v2)
        (and (variable? v1) (variable? v2) (eq? v1 v2)))

    ;; representation of terms
    (define (adjoin-term term term-list)
        (cond ((=zero? term) term-list)
              ((= (order term) (length term-list))
               (cons (coeff term) term-list))
              (else (adjoin-term term
                                 (cons 0 term-list)))))
    (define (the-empty-termlist) '())
    (define (first-term term-list)
        (make-term 
               (- (length term-list) 1)
               (car term-list)))
    (define (rest-term term-list)
        (cdr term-list))
    (define (empty-termlist? term-list) (null? term-list))
    (define (make-term order coeff) (list order coeff))
    (define (order term) (car term))
    (define (coeff term) (cadr term))

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

    ;; interface to rest of the system
    (define (tag p) (attach-tag 'polynomial-dense p))
    (put 'add '(polynomial-dense polynomial-dense)
        (lambda (p1 p2) (tag (add-poly p1 p2))))
    (put 'mul '(polynomial-dense polynomial-dense)
        (lambda (p1 p2) (tag (mul-poly p1 p2))))
    (put 'make 'polynomial-dense
        (lambda (var terms) (tag (make-poly var terms))))
    'done)

; Sparse package
(define (install-polynomial-package-sparse)
    ;; internal procedures
    ;; representation of poly
    (define (make-poly variable term-list) (cons variable term-list))
    (define (variable p) (car p))
    (define (term-list p) (cdr p))
    ; ⟨procedures same-variable? and variable? from section 2.3.2⟩
    (define (variable? x) (symbol? x))
    (define (same-variable? v1 v2)
        (and (variable? v1) (variable? v2) (eq? v1 v2)))

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
    
    ;; interface to rest of the system
    (define (tag p) (attach-tag 'polynomial-sparse p))
    (put 'add '(polynomial-sparse polynomial-sparse)
        (lambda (p1 p2) (tag (add-poly p1 p2))))
    (put 'mul '(polynomial-sparse polynomial-sparse)
        (lambda (p1 p2) (tag (mul-poly p1 p2))))
    (put 'make 'polynomial-sparse
        (lambda (var terms) (tag (make-poly var terms))))
    'done)
