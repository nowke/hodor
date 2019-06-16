(define nil '())
(define (enumarate-interval low high)
    (if (> low high)
        nil
        (cons low (enumarate-interval (+ low 1) high))))
(define (accumulate op initial sequence)
    (if (null? sequence) 
        initial
        (op (car sequence) (accumulate op initial (cdr sequence)))))
(define (divides? a b) (= (remainder b a) 0))
(define (prime? n)
    (define (smallest-divisor n) (find-divisor n 2))
    (define (find-divisor n test-divisor)
        (cond ((> (square test-divisor) n) n)
                ((divides? test-divisor n) test-divisor)
                (else (find-divisor n (+ test-divisor 1)))))
    (= n (smallest-divisor n)))

; Problem: Given positive integer n, find all ordered pairs of distinctive
; integers i, j, where 1 <= j < i <= n, such that i+j is prime
(define (flatmap proc seq)
    (accumulate append nil (map proc seq)))
(define (prime-sum? pair)
    (prime? (+ (car pair) (cadr pair))))
(define (make-pair-sum pair)
    (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
    (map make-pair-sum
         (filter 
            prime-sum?
            (flatmap
                (lambda (i)
                  (map (lambda (j) (list i j))
                       (enumarate-interval 1 (- i 1))))
                (enumarate-interval 1 n)))))

(prime-sum-pairs 6)
; ((2 1 3) (3 2 5) (4 1 5) (4 3 7) (5 2 7) (6 1 7) (6 5 11))

; Problem: Permutations of a set
(define (permutations s)
    (if (null? s)
        (list nil)
        (flatmap 
            (lambda (x)
                (map (lambda (p) (cons x p))
                     (permutations (remove x s))))
            s)))
(define (remove item sequence)
    (filter (lambda (x) (not (= x item)))
            sequence))

(permutations (list 1 2 3))
;Value 3: ((1 2 3) (1 3 2) (2 1 3) (2 3 1) (3 1 2) (3 2 1))

