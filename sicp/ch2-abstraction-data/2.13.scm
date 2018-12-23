; Show that under the assumption of small percentage tolerances 
; there is a simple formula for the approximate percentage tolerance 
; of the product of two intervals in terms of the tolerances of the 
; factors. You may simplify the problem by assuming that all numbers 
; are positive.

; We can show that the resulting percentage of a and b
; as approximately 
;     (percentage a) + (percentage b)

; Note: This proof could be wrong, I have not verified it properly
; 
; Assuming all numbers are positie,
; [xlow, xup] x [ylow, yup] => [xlow * ylow, xup * yup]

; In terms of center "c" and percentage "p",
; interval = [c - c(p/100), c + c(p/100)]

; Multiplying a and b, having center "ca" and percentage "pa"
; [
;    (ca - ca(pa/100)) * (cb - cb(pb/100)),
;    (ca + ca(pa/100)) * (cb + cb(pb/100))
; ]
; [
;    ca * cb * (1 - pa/100) * (1 - pb/100),
;    ca * cb * (1 + pa/100) * (1 + pb/100)
; ]
; [
;    ca * cb * (1 - pa/100 - pb/100 - (pa * pb / 10000)),
;    ca * cb * (1 + pa/100 + pb/100 + (pa * pb / 10000))
; ]

; Ignoring (pa * pb / 10000), since pa and pb are very small,
; we can rewrite the equation as,
; [
;    (ca*cb - (ca*cb)((pa + pb)/100)),
;    (ca*cb + (ca*cb)((pa + pb)/100))
; ]

; Which is of the form we started from, where our new center
; is ~ "ca x cb", and new percentage is "pa + pb"
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

; Hence our new `mul-interval` can be approximated as,
(define (mul-interval x y)
    (make-center-percent
        (* (center x) (center y))
        (+ (percent x) (percent y))))

; Testing
(define interval1 (make-center-percent 5.6 10))
(define interval2 (make-center-percent 1.5 5))
(define interval3 (mul-interval interval1 interval2))
(center interval3) ;Value: 8.399999999999999
(percent interval3) ;Value: 15.