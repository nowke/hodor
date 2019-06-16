; Redefine count-leaves from Section 2.2.2 as an accumulation:
; 
; (define (count-leaves t)
;   (accumulate ⟨??⟩ ⟨??⟩ (map ⟨??⟩ ⟨??⟩)))

(define nil '())
(define (accumulate op initial sequence)
    (if (null? sequence) 
        initial
        (op (car sequence) (accumulate op initial (cdr sequence)))))

(define (enumerate-tree tree)
    (cond ((null? tree) nil)
            ((not (pair? tree)) (list tree))
            (else (append (enumerate-tree (car tree))
                        (enumerate-tree (cdr tree))))))
(define (count-leaves tree)
    (accumulate
        +
        0
        (map (lambda (x) 1) (enumerate-tree tree))))
(count-leaves (list 1 2 (list 3 4 (list 5 6)))) ;Value: 6