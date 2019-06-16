; After considerable work, Alyssa P. Hacker delivers her finished 
; system. Several years later, after she has forgotten all about 
; it, she gets a frenzied call from an irate user, Lem E. Tweakit. 
; It seems that Lem has noticed that the formula for parallel 
; resistors can be written in two algebraically equivalent ways:
;              R1R2 / (R1 + R2)
; and
;            1 / ((1/R1) + (1/R2))
; He has written the following two programs, each of which computes 
; the parallel-resistors formula differently:
(define (par1 r1 r2)
    (div-interval (mul-interval r1 r2)
                  (add-interval r1 r2)))
(define (par2 r1 r2)
    (let ((one (make-interval 1 1)))
        (div-interval
            one (add-interval (div-interval one r1)
                              (div-interval one r2)))))

; Lem complains that Alyssa’s program gives different answers for 
; the two ways of computing. This is a serious complaint.
; Demonstrate that Lem is right. Investigate the behavior of the 
; system on a variety of arithmetic expressions. Make some intervals 
; A and B, and use them in computing the expressions A/A and A/B. 
; You will get the most insight by using intervals whose width is a 
; small percentage of the center value. Examine the results of the 
; computation in center-percent form (see Exercise 2.12).

; Answer
; --------------------------------------

; Let's get the helpers
(define (make-interval a b)
    (cons (min a b) (max a b)))
(define (lower-bound z) (car z))
(define (upper-bound z) (cdr z))
(define (make-center-percent c p)
    (make-center-width c (/ (* c p) 100)))
(define (make-center-width c w) 
    (make-interval (- c w) (+ c w)))
(define (center i)
    (/ (+ (lower-bound i) (upper-bound i)) 2))
(define (width i)
    (/ (- (upper-bound i) (lower-bound i)) 2))
(define (percent i)
    (* (/ (width i) (center i)) 100.0))
(define (add-interval x y)
    (make-interval (+ (lower-bound x) (lower-bound y))
                (+ (upper-bound x) (upper-bound y))))
(define (mul-interval x y)
    (let ((p1 (* (lower-bound x) (lower-bound y)))
          (p2 (* (lower-bound x) (upper-bound y)))
          (p3 (* (upper-bound x) (lower-bound y)))
          (p4 (* (upper-bound x) (upper-bound y))))
     (make-interval (min p1 p2 p3 p4)
                    (max p1 p2 p3 p4))))
(define (div-interval x y) 
    (mul-interval
        x
        (make-interval (/ 1.0 (upper-bound y))
                       (/ 1.0 (lower-bound y)))))
(define (print-interval z)
    (newline)
    (display "[")
    (display (lower-bound z))
    (display ", ")
    (display (upper-bound z))
    (display "]"))

; (i) Test Lem's complaint
; Take 2 resistors [3.5, 3.8] and [10, 10.3]
(define computation1 
    (par1 (make-interval 3.5 3.8) (make-interval 10 10.3)))
(define computation2 
    (par2 (make-interval 3.5 3.8) (make-interval 10 10.3)))
(print-interval computation1) ; [2.48, 2.9]
(print-interval computation2) ; [2.6, 2.77]
; => Lem's complaint is valid, we get two different results

; (ii) Let's define A and B using center-percent form
(define A (make-center-percent 50 5))
(define B (make-center-percent 100 8))

; Operation A/A
(define AA (div-interval A A))
(center AA) ;Value: 1.0050125313283207
(percent AA) ;Value: 9.97506234413964

; Operation A/B
(define AB (div-interval A B))
(center AB) ;Value: .5052334943639292
(percent AB) ;Value: 12.948207171314744

; => As we can observe, center of A/A is not exactly "1"
;   also, center of A/B is not exactly "0.5"
; Hence there's something flawed in our interval operations

; Let's try one more operation 1/A (which I found out helpful
; for next exercise)
(define one (make-center-percent 1 0))
(define 1A (div-interval one A))
(center 1A) ; 2.0050125313283207e-2 (~0.002)
(percent 1A) ; 4.999999999999995 (~5%)

; Also, A/1
(define A1 (div-interval A one))
(center A1) ;Value: 50.
(percent A1) ;Value: 5.