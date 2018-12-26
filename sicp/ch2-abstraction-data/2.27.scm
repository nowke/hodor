; Modify your `reverse` procedure of Exercise 2.18 to produce a `deep-reverse` 
; procedure that takes a list as argument and returns as its value the list
; with its elements reversed and with all sublists deep-reversed as well. 
; For example,
; (define x (list (list 1 2) (list 3 4))) 
; x
; ((1 2) (3 4))
; (reverse x)
; ((3 4) (1 2))
; (deep-reverse x) 
; ((4 3) (2 1))

(define (deep-reverse x)
    (cond ((null? x) x)
          ((not (pair? x)) x)
          (else (append (deep-reverse (cdr x)) (list (deep-reverse (car x)))))))

(define x (list (list 1 2) (list 3 4))) 
(deep-reverse x) ; ((4 3) (2 1))

(define y (list 1 2))
(deep-reverse y) ; (2 1)

(define z (list 1 (list 4 6)))
(deep-reverse z) ; ((6 4) 1)

(define a (list (list 1 2) 3))
(deep-reverse a) ; (3 (2 1))

(define b (list (list (list 6 3) 4) 4 (list 7 8)))
(deep-reverse b) ; ((8 7) 4 (4 (3 6)))