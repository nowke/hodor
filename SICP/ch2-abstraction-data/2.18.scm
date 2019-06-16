; Define a procedure reverse that takes a list as argument and returns 
; a list of the same elements in reverse order:
; (reverse (list 1 4 9 16 25)) 
; (25 16 9 4 1)

(define (append list1 list2)
    (if (null? list1)
        list2
        (cons (car list1) (append (cdr list1) list2))))

; Recursive version
(define (reverse items)
    (if (null? items)
        items
        (append (reverse (cdr items)) (list (car items)))))

(reverse (list 1 4 9 16 25)) ; (25 16 9 4 1)
(reverse (list 1)) ; (1)
(reverse (list 1 7)) ; (7 1)

; Iterative version
(define (reverse items)
    (define (iter a b)
        (if (null? a)
            b
            (iter (cdr a) (cons (car a) b)))
    )
    (iter (cdr items) (list (car items))))

(reverse (list 1 4 9 16 25)) ; (25 16 9 4 1)