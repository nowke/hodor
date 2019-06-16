; Using the results of Exercise 1.16 and Exercise 1.17, 
; devise a procedure that generates an iterative process 
; for multiplying two integers in terms of adding, doubling, 
; and halving and uses a logarithmic number of steps

(define (fast-mul a b)
    (define (even? x) (= (remainder x 2) 0))
    (define (double x) (+ x x))
    (define (halve x) (/ x 2))
    (define (fast-mul-iter a counter sum)
        (cond ((= counter 0) sum)
              ((even? counter) 
                (fast-mul-iter (double a) (halve counter) sum))
              (else 
                (fast-mul-iter a (- counter 1) (+ a sum)))
        )
    )
    (fast-mul-iter a b 0)
)

(fast-mul 2 6) ;Value: 12
(fast-mul 24 67) ;Value: 1608
(fast-mul 1 7) ;Value: 7