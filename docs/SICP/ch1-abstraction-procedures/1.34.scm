; Suppose we define the procedure
(define (f g) (g 2))
; Then we have

(f square) ;Value: 4
(f (lambda (z) (* z (+ z 1)))) ;Value: 6

; What happens if we (perversely) ask the interpreter 
; to evaluate the combination (f f)? Explain

; Answer
; ------------------
; Evaluation of (f f) will give the following error
; "The object 2 is not applicable."
; 
; This is because, it will be evaluated to (f 2) => (2 2)
; Hence, interpreter will throw the error