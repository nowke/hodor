; Design a procedure that evolves an iterative 
; exponentiation process that uses successive squaring
; and uses a logarithmic number of steps, as does `fast-expt`
; (Hint: Using the observation that (bn/2)2 = (b2)n/2, keep, 
; along with the exponent n and the base b, an additional state variable a, 
; and define the state transformation in such a way that the product ab^n is 
; unchanged from state to state. At the beginning of the process a is taken to be 1, 
; and the answer is given by the value of a at the end of the process. 
; In general, the technique of defining an invariant quantity that remains unchanged 
; from state to state is a powerful way to think about the design of 
; iterative algorithms.)
(define (fast-expt b n)
    (define (even? n) (= (remainder n 2) 0))
    (define (fast-expt-iter base counter prod)
        (cond ((= counter 0) prod)
              ((even? counter) (fast-expt-iter (square base) (/ counter 2) prod))
              (else (fast-expt-iter base (- counter 1) (* base prod)))
        )
    )
    (fast-expt-iter b n 1)
)

(fast-expt 2 10) ;Value: 1024
(fast-expt 2 5) ;Value: 32
(fast-expt 2 1000) ;Value: 1071508607186267320948425049
;0600018105614048117055336074437503883703510511249361224931
;98378815695858127594672917553146825187145285692314043598457
;757469857480393456777482423098542107460506237114187795418215
;30464749835819412673987675591655439460770629145711964776865421
;67660429831652624386837205668069376
