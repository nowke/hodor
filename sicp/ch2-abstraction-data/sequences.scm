; List
(define mylist (list 1 2 3 4))

(car mylist) ;Value: 1
(cdr mylist) ;Value 2: (2 3 4)

(cadr mylist) ;Value: 2

; Implementation of `list-ref`
(define (list-ref items n)
    (if (= n 0)
        (car items)
        (list-ref (cdr items) (- n 1))))

(define squares (list 1 4 9 16 25))
(list-ref squares 3) ;Value: 16

; Length of a list
(define (length items)
    (if (null? items)
        0
        (+ 1 (length (cdr items)))))

(define odds (list 1 3 5 7))
(length odds) ;Value: 4

; Iterative length
(define (length items)
    (define (iter a count)
        (if (null? a)
            count
            (iter (cdr a) (+ 1 count))))
    (iter items 0))
(define evens (list 2 4 6 8))
(length evens) ;Value: 4

; Append procedure
(define (append list1 list2)
    (if (null? list1)
        list2
        (cons (car list1) (append (cdr list1) list2))))

(append odds evens) ;(1 3 5 7 2 4 6 8)