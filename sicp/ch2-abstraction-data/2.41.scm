; Write a procedure to find all ordered triples of distinct positive integers 
; i, j, and k less than or equal to a given integer n that sum to a given 
; integer s.

; Define helpers
(define nil '())
(define (enumarate-interval low high)
    (if (> low high)
        nil
        (cons low (enumarate-interval (+ low 1) high))))
(define (accumulate op initial sequence)
    (if (null? sequence) 
        initial
        (op (car sequence) (accumulate op initial (cdr sequence)))))
(define (flatmap proc seq)
    (accumulate append nil (map proc seq)))

; Construct `triplets-of-sum`
(define (triplets-of-sum n s)
    (define enumerate-n (enumarate-interval 1 n))
    (filter 
        (lambda (x) (= (accumulate + 0 x) s))
        (flatmap
            (lambda (i)
                (flatmap
                    (lambda (j)
                        (map
                            (lambda (k) (list i j k))
                            (enumarate-interval 1 (- j 1))))
                    (enumarate-interval 1 (- i 1))))
            enumerate-n)))

(triplets-of-sum 6 8) ;((4 3 1) (5 2 1))
(triplets-of-sum 6 9) ;((4 3 2) (5 3 1) (6 2 1))