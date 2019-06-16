; Fibonacci series using tree recursion
(define (fib n)
    (cond ((= n 0) 0)
          ((= n 1) 1)
          (else (+ (fib (- n 1))
                   (fib (- n 2))))
    )
)

(fib 6)
(fib 5)
(fib 4)

; Fibonacci series using iterative process
(define (fib n)
    (define (fib-iter a b count)
        (if (= count 0)
            b
            (fib-iter (+ a b) a (- count 1))
        )
    )
    (fib-iter 1 0 n)
)
(fib 6)
(fib 5)
(fib 4)
; (fib 6)
;  => (fib-iter 1 0 6)
;  => (fib-iter 1 1 5)
;  => (fib-iter 2 1 4)
;  => (fib-iter 3 2 3)
;  => (fib-iter 5 3 2)
;  => (fib-iter 8 5 1)
;  => (fib-iter 13 8 0)
;  => 8