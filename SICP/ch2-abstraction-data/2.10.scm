; Ben Bitdiddle, an expert systems programmer, looks over Alyssa’s 
; shoulder and comments that it is not clear what it means to divide 
; by an interval that spans zero. Modify Alyssa’s code to check for 
; this condition and to signal an error if it occurs.

(define (make-interval a b) (cons a b))
(define (lower-bound z) (car z))
(define (upper-bound z) (cdr z))

; Check for spanning zero condition while dividing
(define (div-interval x y) 
    (if (<= (* (lower-bound y) (upper-bound y)) 0)
        (error "Cannot divide interval spanning zero") 
        (mul-interval
            x
            (make-interval (/ 1.0 (upper-bound y))
                            (/ 1.0 (lower-bound y))))))

; This will produce error
(div-interval 
    (make-interval 1 1.5) 
    (make-interval -0.5 0.5)) ;Cannot divide interval spanning zero