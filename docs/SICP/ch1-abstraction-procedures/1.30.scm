; The sum procedure above generates a linear recursion. 
; The procedure can be rewritten so that the sum is 
; performed iteratively. Show how to do this by filling 
; in the missing expressions in the following definition:
;
; (define (sum term a next b) 
;   (define (iter a result)
;       (if ⟨??⟩ 
;           ⟨??⟩
;           (iter ⟨??⟩ ⟨??⟩))) 
;   (iter ⟨??⟩ ⟨??⟩))    
(define (sum term a next b)
    (define (iter a result)
        (if (> a b)
            result
            (iter (next a) (+ result (term a)))))
    (iter a 0))

(define (pi-sum a b) 
    (define (pi-term x)
        (/ 1.0 (* x (+ x 2)))) 
    (define (pi-next x)
        (+ x 4))
    (sum pi-term a pi-next b))

(* 8 (pi-sum 1 1000)) ;Value: 3.139592655589782
