; Section 2.3.2 described a program that performs symbolic differentiation:

(define (deriv exp var) 
    (cond ((number? exp) 0)
          ((variable? exp)
            (if (same-variable? exp var) 1 0))
          ((sum? exp)
               (make-sum (deriv (addend exp) var)
                         (deriv (augend exp) var)))
          ((product? exp)
                (make-sum (make-product
                            (multiplier exp)
                            (deriv (multiplicand exp) var))
                          (make-product
                            (deriv (multiplier exp) var)
                            (multiplicand exp))))
          (else (error "unknown expression type: 
                       DERIV" exp))))

; We can regard this program as performing a dispatch on the type of the expression 
; to be differentiated. In this situation the “type tag” of the datum is the 
; algebraic operator symbol (such as +) and the operation being performed is deriv. 
; We can transform this program into data-directed style by rewriting the basic 
; derivative procedure as

(define (deriv exp var) 
    (cond ((number? exp) 0)
          ((variable? exp) (if (same-variable? exp var) 1 0)) 
          (else ((get 'deriv (operator exp))
                 (operands exp) var)))) 
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

; (a) Explain what was done above. Why can’t we assimilate the predicates number? 
;     and variable? into the data-directed dispatch?

; Ans:
; number? and variable? do not use any operator or operands. Hence there's no need
; of dispatch method

; (b) Write the procedures for derivatives of sums and products, and the auxiliary 
;     code required to install them in the table used by the program above

; Ans:

(define (deriv-sum exp var)
    (make-sum (deriv (addend exp) var)
              (deriv (augend exp) var)))
(define (deriv-product exp var)
    (make-sum (make-product 
                (multiplier exp)
                (deriv (multiplicand exp) var))
              (make-product
                (deriv (multiplier exp) var)
                (multiplicand exp))))

(deriv (install-deriv)
    (put 'deriv '+ deriv-sum)
    (put 'deriv '* deriv-product)
    'done)

; (c) Choose any additional differentiation rule that you like, such as the one for 
;     exponents (Exercise 2.56), and install it in this data-directed system.

; Ans:
(define (deriv-expo exp var)
    (define b (base exp))
    (define e (exponent exp))
    (make-product
        e
        (make-product
            (make-exp b (make-sum e -1))
            (deriv b var))))

(define (install-expo)
    (put 'deriv '** deriv-expo))

; (d) In this simple algebraic manipulator the type of an expression is the algebraic
;     operator that binds it together. Suppose, however, we indexed the procedures 
;     in the opposite way, so that the dispatch line in `deriv` looked like
;  
;     ((get (operator exp) 'deriv) (operands exp) var)
;   
;     What corresponding changes to the derivative system are required?

; Ans:
; We need to reverse the map
(put '+ 'deriv deriv-sum)
(put '* 'deriv deriv-product)
