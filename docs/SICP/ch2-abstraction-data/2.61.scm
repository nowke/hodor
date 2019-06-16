; Give an implementation of adjoin-set using the ordered representation. By 
; analogy with element-of-set? show how to take advantage of the ordering to 
; produce a procedure that requires on the average about half as many steps as 
; with the unordered representation.

(define (adjoin-set x set)
    (cond ((null? set) (list x))
          ((= (car set) x) set)
          ((> (car set) x) (cons x set))
          (else (cons (car set) (adjoin-set x (cdr set))))))

; Testing

(adjoin-set 5 (list 1 3 4 9))
; (1 3 4 5 9)

(adjoin-set 5 (list 1 3 4 5))
; (1 3 4 5)