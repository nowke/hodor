; Complete the following definitions of reverse (Exercise 2.18) in terms of 
; fold-right and fold-left from Exercise 2.38:
; (define (reverse sequence)
;   (fold-right (lambda (x y) ⟨??⟩) nil sequence))
; (define (reverse sequence)
;   (fold-left (lambda (x y) ⟨??⟩) nil sequence))

(define nil '())
(define (accumulate op initial sequence)
    (if (null? sequence) 
        initial
        (op (car sequence) (accumulate op initial (cdr sequence)))))
(define fold-right accumulate)
(define (fold-left op initial sequence)
    (define (iter result rest)
        (if (null? rest)
            result
            (iter (op result (car rest))
                  (cdr rest))))
    (iter initial sequence))

(define (reverse sequence)
    (fold-right (lambda (x y) (append y (list x))) nil sequence))
(reverse (list 1 2 3 4)) ;Value 2: (4 3 2 1)

(define (reverse sequence)
    (fold-left (lambda (x y) (cons y x)) nil sequence))
(reverse (list 1 2 3 4)) ;Value 3: (4 3 2 1)