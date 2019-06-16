; A directed line segment in the plane can be represented as a pair of vectors — 
; the vector running from the origin to the start-point of the segment, and the 
; vector running from the origin to the end-point of the segment. Use your vector 
; representation from Exercise 2.46 to define a representation for segments with 
; a constructor make-segment and selectors start-segment and end-segment.

(define (make-vect x y) (cons x y))

(define (make-segment v1 v2) (cons v1 v2))
(define (start-segment segment) (car segment))
(define (end-segment segment) (cdr segment))

; Testing
(define s1 (make-segment
            (make-vect 1 2)
            (make-vect 2 3)))
(start-segment s1) ; (1 . 2)
(end-segment s1) ; (2 . 3)