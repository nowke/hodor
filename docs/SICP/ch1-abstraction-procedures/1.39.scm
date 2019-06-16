; A continued fraction representation of the tangent 
; function was published in 1770 by the German mathematician 
; J.H. Lambert:
;  tan x =       x
;          --------------
;          1 -    x^2
;              ----------
;              3 -  x^2
;                  -------
;                  5 - ... 
;  where x is in radians. Define a procedure (tan-cf x k) 
; that computes an approximation to the tangent function 
; based on Lambert’s formula. k specifies the number of terms 
; to compute, as in Exercise 1.37.
(define (cont-frac n d k)
    (define (iter k res)
        (if (= k 0)
            res
            (iter (- k 1)
                  (/ (n k) (+ (d k) res)))))
    (iter k 0))

(define (tan-cf x k)
    (cont-frac (lambda (i) (if (= i 1) x (- (square x))))
               (lambda (i) (- (* 2 i) 1))
               k))

(tan-cf 0.7853981634 20) ;Value: 1.0000000000051033 (tan(45 deg) = 1)