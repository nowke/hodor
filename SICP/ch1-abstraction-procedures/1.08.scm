; Newton’s method for cube roots is based on the fact 
; that if y is an approximation to the cube root of x , 
; then a better approximation is given by the value
;
;            x/y² + 2y
;           ------------
;                 3
;
; Use this formula to implement a cube-root procedure analogous 
; to the square-root procedure
(define (square x) (* x x))
(define (cube x) (* x x x))
(define (improve guess x)
    (/ (+ (/ x (square guess)) (* 2 guess)) 3)
)

(define (good-enough? guess prev-guess)
  (< (abs (- guess prev-guess)) (abs (* guess 0.001)))
)
(define (cuberoot-iter guess prev-guess x) 
    (if (good-enough? guess prev-guess)
      guess
      (cuberoot-iter (improve guess x) guess x)
    )
  )

(define (cube-root x) 
    (cuberoot-iter 1.0 0.0 x)
) 

(cube-root 8) ;Value: 2.000000000012062
(cube-root 1e-27) ;Value: 1.0000000000037844e-9
(cube-root 1e15) ;Value: 100000.0000002152