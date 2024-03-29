; Extend the differentiation program to handle sums and products of arbitrary 
; numbers of (two or more) terms. Then the last example above could be expressed 
; as
; (deriv '(* x y (+ x 3)) 'x)
; Try to do this by changing only the representation for sums and products, 
; without changing the deriv procedure at all. For example, the addend of a sum 
; would be the first term, and the augend would be the sum of the rest of the terms.

; New definitions for `augend` and `multiplcand` are,
; ====================================================
(define (augend s)
    (if (null? (cdddr s))
        (caddr s)
        (make-sum (caddr s) (cadddr s))))

(define (multiplicand p)
    (if (null? (cdddr p))
        (caddr p)
        (make-product (caddr p) (cadddr p))))
; ====================================================

; Testing
(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))
(define (=number? exp num) 
    (and (number? exp) (= exp num)))
(define (sum? x) (and (pair? x) (eq? (car x) '+)))
(define (addend s) (cadr s))
(define (make-sum a1 a2) 
    (cond ((=number? a1 0) a2) 
            ((=number? a2 0) a1)
            ((and (number? a1) (number? a2)) (+ a1 a2))
            (else (list '+ a1 a2))))

(define (product? x) (and (pair? x) (eq? (car x) '*)))
(define (multiplier p) (cadr p))
(define (make-product m1 m2) 
    (cond ((or (=number? m1 0) (=number? m2 0)) 0)
            ((=number? m1 1) m2)
            ((=number? m2 1) m1)
            ((and (number? m1) (number? m2)) (* m1 m2))
            (else (list '* m1 m2))))

(define (deriv exp var)
    (cond ((number? exp) 0)
          ((variable? exp) (if (same-variable? exp var) 1 0))
          ((sum? exp) (make-sum (deriv (addend exp) var)
                                (deriv (augend exp) var)))
          ((product? exp)
            (make-sum
                (make-product 
                    (multiplier exp)
                    (deriv (multiplicand exp) var))
                (make-product
                    (deriv (multiplier exp) var)
                    (multiplicand exp))))
          (else
            (error "unknown expression type: DERIV" exp))))

(deriv '(* x y (+ x 3)) 'x)
; (+ (* x y) (* y (+ x 3)))

(deriv '(* x y (+ x 3 y)) 'x)
; (+ (* x y) (* y (+ x 3 y)))