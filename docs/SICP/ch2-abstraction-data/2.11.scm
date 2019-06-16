; In passing, Ben also cryptically comments: “By testing the 
; signs of the endpoints of the intervals, it is possible to 
; break mul-interval into nine cases, only one of which 
; requires more than two multiplications.” Rewrite this procedure 
; using Ben’s suggestion

; Answer
; ------------------------------------
; The nine cases are based on 3 signs of x and y
; x can have [-ve, -ve], [-ve, +ve] and [+ve, +ve] (3)
;  (Note: [+ve, -ve] is NOT possible since order is always [lower, upper])
; y can have [-ve, -ve], [-ve, +ve] and [+ve, +ve] (3)
; Hence there can be 3x3 = 9 possibilities.

; Let's associate a value to each combination
; [-ve, -ve] => -1, [-ve, +ve] => 0, [+ve, +ve] => 1
; and define a function that returns us this value
(define (get-pair-value low up)
    (cond ((and (< low 0) (< up 0)) -1)
          ((and (< low 0) (> up 0)) 0)
          (else 1)))

; We can modify our `make-interval` to make sure always (a, b) has
; the property a < b
(define (make-interval a b)
    (cons (min a b) (max a b)))
(define (lower-bound z) (car z))
(define (upper-bound z) (cdr z))
(define (print-interval z)
    (newline)
    (display "[")
    (display (lower-bound z))
    (display ", ")
    (display (upper-bound z))
    (display "]"))

; Let's rewrite our `mul-interval` procedure
(define (mul-interval x y)
    (define xlow (lower-bound x))
    (define xup  (upper-bound x))
    (define ylow (lower-bound y))
    (define yup  (upper-bound y))

    (define xval (get-pair-value xlow xup))
    (define yval (get-pair-value ylow yup))

    (cond ((= xval -1)
           (cond ((= yval -1)            ; [-ve, -ve], [-ve, -ve]
                  (make-interval (* xup yup) (* xlow ylow)))
                 ((= yval 0)             ; [-ve, -ve], [-ve, +ve]
                  (make-interval (* xlow yup) (* xlow ylow)))
                 (else                   ; [-ve, -ve], [+ve, +ve]
                  (make-interval (* xlow yup) (* xup ylow)))))
          ((= xval 0)
           (cond ((= yval -1)            ; [-ve, +ve], [-ve, -ve]
                  (make-interval (* xup ylow) (* xlow ylow)))
                 ((= yval 0)             ; [-ve, +ve], [-ve, +ve]
                  (make-interval 
                    (min (* xlow yup)  (* xup ylow)) 
                    (max (* xlow ylow) (* xup yup))))
                 (else                   ; [-ve, +ve], [+ve, +ve]
                  (make-interval (* xlow yup) (* xup yup)))))
          (else
           (cond ((= yval -1)            ; [+ve, +ve], [-ve, -ve]
                  (make-interval (* xup ylow) (* xlow yup)))
                 ((= yval 0)             ; [+ve, +ve], [-ve, +ve]
                  (make-interval (* xup ylow) (* xup yup)))
                 (else                   ; [+ve, +ve], [+ve, +ve]
                  (make-interval (* xlow ylow) (* xup yup)))))))

; Testing
(print-interval (mul-interval 
    (make-interval -1.5 -1.2) 
    (make-interval -1   2))) ; [-3., 1.5]

(print-interval (mul-interval 
    (make-interval -5 3) 
    (make-interval -2 4)))   ; [-20, 12]  