; Observe that our model of evaluation allows for combinations 
; whose operators are compound expressions. Use this 
; observation to describe the behavior of the following procedure:
;
(define (a-plus-abs-b a b) 
    ((if (> b 0) + -) a b)
)
; The above function computes a + |b|
; i.e. if b is +ve -> a + b
;      if b is -ve -> a - b
;
; Here the operation type itself is determined conditionally
; (+ a b) OR (- a b)
