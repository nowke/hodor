; The width of an interval is half of the difference between its 
; upper and lower bounds. The width is a measure of the uncertainty 
; of the number specified by the interval. For some arithmetic 
; operations the width of the result of combining two intervals is 
; a function only of the widths of the argument intervals, whereas 
; for others the width of the combination is not a function of the 
; widths of the argument intervals. Show that the width of the sum 
; (or difference) of two intervals is a function only of the widths 
; of the intervals being added (or subtracted). Give examples to 
; show that this is not true for multiplication or division.

(define (make-interval a b) (cons a b))
(define (lower-bound z) (car z))
(define (upper-bound z) (cdr z))
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

; Define `width`
(define (width-interval z)
    (/ (- (upper-bound z) (lower-bound z)) 2))

; (i) Testing with addition
;   width1 = (1.8-1.5)/2 = 0.15
;   width2 = (5.75-5.6)/2 = 0.075
;   width1 + width2 = 0.225
;   also, width-add = (7.55 - 7.1)/2 = 0.225
(define interval1 (add-interval
                    (make-interval 1.5 1.8)
                    (make-interval 5.6 5.75)))
(width-interval interval1) ;Value: .2250000000000001
; => width of the sum (or difference) of two intervals is a 
;    function only of the widths of the intervals being added 
;    (or subtracted)

; (ii) Testing with multiplication
;    width1 = 0.15, width2 = 0.075 => width1 * width2 = 0.01125
;   also, width-mul = (10.35 - 8.4)/2 = 0.975
(define interval2 (mul-interval
                    (make-interval 1.5 1.8)
                    (make-interval 5.6 5.75)))
(width-interval interval2) ;Value: .9750000000000005
; => width of the multiplication (or division) of two intervals is
;    "not" a function of widths of intervals being multiplied
;    (or divided)