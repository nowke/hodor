; A function f is defined by the rule that
; 
; f(n) = n if n < 3
;      = f(n-1) + 2f(n-2) + 3f(n-3) if n >= 3
;
; Write a procedure that computes f by means of a recursive process. 
; Write a procedure that computes f by means of an iterative process.
(define (f-rec n) 
    (cond ((< n 3) n)
          (else (+ (f-rec (- n 1))
                   (* 2 (f-rec (- n 2)))
                   (* 3 (f-rec (- n 3))))))
)

(define (f-iter n)
    (define (compute a b c)
        (+ c (* 2 b) (* 3 a)))
    (define (f-iter-inner a b c count)
        (cond ((= count 2) c)
              ((< count 2) count)
              (else (f-iter-inner b c (compute a b c) (- count 1)))))
    (f-iter-inner 0 1 2 n)
)

(f-rec 5) ;Value: 25
(f-rec 4) ;Value: 11
(f-rec 3) ;Value: 4
(f-rec 2) ;Value: 2
(f-rec 1) ;Value: 1
(f-rec 0) ;Value: 0

(f-iter 5) ;Value: 25
(f-iter 4) ;Value: 11
(f-iter 3) ;Value: 4
(f-iter 2) ;Value: 2
(f-iter 1) ;Value: 1
(f-iter 0) ;Value: 0