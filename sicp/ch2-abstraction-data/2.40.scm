; Define a procedure unique-pairs that, given an integer n, generates the sequence 
; of pairs (i, j) with 1 ≤ j < i ≤ n. Use unique-pairs to simplify the definition 
; of prime-sum-pairs given above.

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

; Construct `unique-pairs` procedure
(define (unique-pairs n)
    (flatmap
        (lambda (i)
            (map 
                (lambda (j) (list i j))
                (enumarate-interval 1 (- i 1))))
        (enumarate-interval 1 n)))

(unique-pairs 6)
;Value 2: ((2 1) (3 1) (3 2) (4 1) (4 2) (4 3) (5 1) (5 2) (5 3) (5 4) (6 1) (6 2) (6 3) (6 4) (6 5))
