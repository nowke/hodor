; An infinite continued fraction is an expression of the form
; f =        N1
;     -----------------
;     D1 +     N2
;          -------------
;          D2 +   N3
;               --------
;               D3 + ...
; As an example, one can show that the infinite continued 
; fraction expansion with the Ni and the Di all equal to 1 produces 
; 1/φ, where φ is the golden ratio (described in Section 1.2.2). 
; One way to approximate an infinite continued fraction is to 
; truncate the expansion after a given number of terms. Such a 
; truncation — a so-called k-term finite continued fraction — has the form
;       N1
; ---------------
; D1 +   N2
;      ---------
;       ... + Nk/Dk
;
; Suppose that n and d are procedures of one argument (the term index i) 
; that return the Ni and Di of the terms of the continued fraction. 
; Define a procedure cont-frac such that evaluating (cont-frac n d k) 
; computes the value of the k-term finite continued fraction. Check 
; your procedure by approximating 1/φ usng
; (cont-frac (lambda (i) 1.0) (lambda (i) 1.0) k)
; for successive values of k. How large must you make k in order to get 
; an approximation that is accurate to 4 decimal places?

(define (cont-frac n d k)
    (if (= k 0) 
        0
        (/ (n 1)
           (+ (d 1)
              (cont-frac (lambda (x) (n (+ x 1)))
                         (lambda (x) (n (+ x 1)))
                         (- k 1))))))

; 1/φ = 0.6180339887
; At around k = 12, we get value accurate to 4 decimal placess
(cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 12) ; Value: .6180257510729613
