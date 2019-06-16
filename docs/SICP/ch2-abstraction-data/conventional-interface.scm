; Sum of squares of odd tree leaves
(define (sum-odd-squares tree)
    (cond ((null? tree) 0)
          ((not (pair? tree))
            (if (odd? tree) (square tree) 0))
          (else (+ (sum-odd-squares (car tree))
                   (sum-odd-squares (cdr tree))))))

(sum-odd-squares 
    (list 1
        (list 2 (list 3 4) 5)
        (list 6 7))) ;Value: 84 (1 + 9 + 25 + 49)


; Even fibonacci numbers
(define (fib n)
    (define (iter a b count)
        (if (= count 0)
            b
            (iter (+ a b) a (- count 1))))
    (iter 1 0 n))
(define nil '())
(define (even-fibs n)
    (define (next k)
        (if (> k n)
           nil
           (let ((f (fib k)))
             (if (even? f)
                 (cons f (next (+ k 1)))
                 (next (+ k 1))))))
    (next 0))
(even-fibs 10) ; (0 2 8 34)

; Sequence operations
; Defining filter
(define (fitler predicate sequence)
    (cond ((null? sequence) nil)
          ((predicate (car sequence))
            (cons (car sequence)
                  (filter predicate (cdr sequence))))
          (else (filter predicate sequence))))
(filter odd? (list 1 2 3 4 5 6)) ; (1 3 5)

; Defining accumulation
(define (accumulate op initial sequence)
    (if (null? sequence) 
        initial
        (op (car sequence) (accumulate op initial (cdr sequence)))))
(accumulate + 0 (list 1 2 3 4)) ;Value: 10
(accumulate * 1 (list 1 2 3 4)) ;Value: 24
(accumulate cons nil (list 1 2 3 4 5)) ;(1 2 3 4 5)

; Enumerating integers
(define (enumarate-interval low high)
    (if (> low high)
        nil
        (cons low (enumarate-interval (+ low 1) high))))
(enumarate-interval 2 7) ; (2 3 4 5 6 7)

; Enumerating tree
(define (enumerate-tree tree)
    (cond ((null? tree) nil)
          ((not (pair? tree)) (list tree))
          (else (append (enumerate-tree (car tree))
                        (enumerate-tree (cdr tree))))))
(enumerate-tree (list 1 (list 2 (list 3 4)) 5)) ; (1 2 3 4 5)

; Rewriting `sum-odd-squares`
(define (sum-odd-squares tree)
    (accumulate
        +
        0
        (map square (filter odd? (enumerate-tree tree)))))
(sum-odd-squares 
    (list 1
        (list 2 (list 3 4) 5)
        (list 6 7))) ;Value: 84

; Rewriting `even-fibs`
(define (even-fibs k)
    (accumulate
      cons
      nil
      (filter even? (map fib (enumarate-interval 0 k)))))
(even-fibs 10) ; (0 2 8 34)

(define (list-fib-squares n)
    (accumulate
        cons
        nil
        (map square (map fib (enumarate-interval 0 n)))))
(list-fib-squares 10) ; (0 1 1 4 9 25 64 169 441 1156 3025)