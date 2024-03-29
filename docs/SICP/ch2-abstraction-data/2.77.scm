; Louis Reasoner tries to evaluate the expression (magnitude z) where z is the 
; object shown in Figure 2.24. To his surprise, instead of the answer 5 he gets 
; an error message from `apply-generic`, saying there is no method for the operation
; magnitude on the types (complex). He shows this interaction to Alyssa P. Hacker, 
; who says “The problem is that the complex-number selectors were never defined for 
; complex numbers, just for polar and rectangular numbers. All you have to do to make
; this work is add the following to the complex package:”
; ```
; (put 'real-part '(complex) real-part)
; (put 'imag-part '(complex) imag-part)
; (put 'magnitude '(complex) magnitude)
; (put 'angle '(complex) angle)
; ```
; Describe in detail why this works. As an example, trace through all the procedures 
; called in evaluating the expression `(magnitude z)` where `z` is the object shown in 
; Figure 2.24. In particular, how many times is `apply-generic` invoked? What procedure
; is dispatched to in each case?

; Testing
(load "arithmetic-package.scm")

(put 'real-part '(complex) real-part)
(put 'imag-part '(complex) imag-part)
(put 'magnitude '(complex) magnitude)
(put 'angle '(complex) angle)

(define z (make-complex-from-real-imag 3 4))
(magnitude z) ;Value: 5

; Answer
; ---------------
; We cannot interact with methods not defined as interface in the "arithmetic-package"
; Hence, adding the above lines makes it accessible for complex type

; (make-complex-from-real-imag 3 4)
; => (complex rectangular 3 4)
; 
; (magnitude z)
; => (magnitude '(complex rectangular 3 4))
; => (apply-generic 'magnitude '(complex rectangular 3 4))
; => (apply magnitude '(rectangular 3 4))
; => (magnitude '(rectangular 3 4))
; => (apply-generic 'magnitude '(rectangular 3 4))
; => (apply (lambda (z) (sqrt (+ (square (real-part z)) (square (imag-part z)))))
;           '(3 4))
; => 5

; `apply-generic` is calle twice