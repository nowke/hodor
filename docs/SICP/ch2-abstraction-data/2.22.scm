; Louis Reasoner tries to rewrite the first square-list procedure of 
; Exercise 2.21 so that it evolves an iterative process:
(define nil '())
(define (square-list items) 
    (define (iter things answer)
        (if (null? things) 
            answer
            (iter (cdr things)
                  (cons (square (car things))
                        answer))))
    (iter items nil))
(square-list (list 1 2 3 4)) ; (16 9 4 1)

; Unfortunately, defining square-list this way produces the answer list 
; in the reverse order of the one desired. Why? Louis then tries to fix 
; his bug by interchanging the arguments to cons:
(define (square-list items)
    (define (iter things answer) 
        (if (null? things)
            answer
            (iter (cdr things)
                  (cons answer
                        (square (car things))))))
    (iter items nil))

(square-list (list 1 2 3 4)) ; ((((() . 1) . 4) . 9) . 16)
; This doesn’t work either. Explain.

; ====================================
; Answer
; ====================================
; (i) Prints in reverse order
;  => Let's traverse with (1 2 3 4)
; round      things          answer
;   1       (1 2 3 4)          ()
;             (2 3 4)        (cons (square 1) ())
;             (2 3 4)          (1)
;   2           (3 4)        (cons (square 2) (1))
;               (3 4)        (cons 4 (1))
;               (3 4)        (4 1)
;   3             (4)        (cons (square 3) (4 1))
;                 (4)        (9 4 1)
;   4              ()        (16 9 4 1)

; (ii) Prints nested structures
; round       things         answer
;  1         (1 2 3 4)         ()
;              (2 3 4)         (cons () (square 1))
;              (2 3 4)         (cons () 1)
;              (2 3 4)         (() . 1)
;  2             (3 4)         (cons (() . 1) (square 2))
;                (3 4)         ((() . 1) . 4)
;  ...                         ...
;                              ((((() . 1) . 4) . 9) . 16)

; To fix this, we must append two lists instead of `cons`