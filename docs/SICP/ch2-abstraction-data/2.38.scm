; The accumulate procedure is also known as fold-right, because it combines 
; the first element of the sequence with the result of combining all the 
; elements to the right. There is also a fold-left, which is similar to fold-right, 
; except that it combines elements working in the opposite direction:
(define (fold-left op initial sequence)
    (define (iter result rest)
        (if (null? rest)
            result
            (iter (op result (car rest))
                  (cdr rest))))
    (iter initial sequence))

; What are the values of
; (fold-right / 1 (list 1 2 3))
; (fold-left / 1 (list 1 2 3))
; (fold-right list nil (list 1 2 3))
; (fold-left list nil (list 1 2 3))    
;
; Give a property that op should satisfy to guarantee that fold-right and fold-left 
; will produce the same values for any sequence.

(define nil '())
(define (accumulate op initial sequence)
    (if (null? sequence) 
        initial
        (op (car sequence) (accumulate op initial (cdr sequence)))))
(define fold-right accumulate)


(fold-right / 1 (list 1 2 3)) ;Value: 3/2 (3/2/1)
(fold-left / 1 (list 1 2 3)) ;Value: 1/6 (1/2/3)
(fold-right list nil (list 1 2 3)) ; (1 (2 (3 ())))
(fold-left list nil (list 1 2 3)) ; (((() 1) 2) 3)

; For `fold-left` and `fold-right` to produce same result, 
; `op` should be Commutative and Associative