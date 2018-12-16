; Alyssa P. Hacker complains that we went to a lot of 
; extra work in writing expmod. After all, she says, 
; since we already know how to compute exponentials, 
; we could have simply written
; 
(define (expmod base exp m) 
    (remainder (fast-expt base exp) m))
; 
; Is she correct? Would this procedure serve as well for 
; our fast prime tester? Explain.
(define (even? x) (= (remainder x 2) 0))
(define (expmod-prev base exp m)
    (cond ((= exp 0) 1)
          ((even? exp)
           (remainder
            (square (expmod-prev base (/ exp 2) m))
             m)) 
          (else
            (remainder
             (* base (expmod-prev base (- exp 1) m))
             m))))
(define (fast-expt b n)
    (define (even? n) (= (remainder n 2) 0))
    (cond ((= n 0) 1)
          ((even? n) (square (fast-expt b (/ n 2))))
          (else (* b (fast-expt b (- n 1))))))

; Let's test whether both `expmod-prev` and `expmod` return
; correct result for some values
(expmod-prev 6 97 97) ;Value: 6
(expmod 6 97 97)      ;Value: 6

(expmod-prev 103 1000003 1000003) ;Value: 103
(expmod 103 1000003 1000003)      ;Value: 103

; We can also observe that the current `expmod` takes more
; time for large numbers, as it computes very large
; intermediate values