; We can represent a set as a list of distinct elements, and we can represent the 
; set of all subsets of the set as a list of lists. For example, if the set is 
; (1 2 3), then the set of all subsets is (() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3)). 
; Complete the following definition of a procedure that generates the set of subsets
;  of a set and give a clear explanation of why it works:
; 
; (define (subsets s) 
;     (if (null? s)
;         (list nil)
;         (let ((rest (subsets (cdr s))))
;           (append rest (map ⟨??⟩ rest)))))

(define (subsets s) 
    (if (null? s)
        (list '())
        (let ((rest (subsets (cdr s))))
          (append rest 
                  (map (lambda (x) (cons (car s) x)) 
                       rest)))))

(subsets 
    (list 1 2 3)) ; (() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3))

; Explanation
; ==================================
; Let's say S = (1, 2, 3)
; In our recursive step,
;   rest = subset (2, 3)
;  subset (2, 3) is actually => {(), (2), (3), (2, 3)} 
; Also (car S) = 1
; If we append subset (2, 3) with "1" added to each element of subset(2, 3)
; we get back subset (1, 2, 3)

; i.e.
; x = (1) => subset(2, 3) => {(1), (1 2), (1 3), (1 2 3)}
; y = subset(2, 3)
; x + y = {(1), (1 2), (1 3), (1 2 3), (), (2), (3), (2 3)}
