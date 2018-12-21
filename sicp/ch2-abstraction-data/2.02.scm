; Consider the problem of representing line segments in a plane. 
; Each segment is represented as a pair of points: a starting point 
; and an ending point. Define a constructor make-segment and 
; selectors start-segment and end-segment that define the representation 
; of segments in terms of points. Furthermore, a point can be represented 
; as a pair of numbers: the x coordinate and the y coordinate. 
; Accordingly, specify a constructor make-point and selectors x-point and 
; y-point that define this representation. Finally, using your selectors 
; and constructors, define a procedure midpoint-segment that takes a 
; line segment as argument and returns its midpoint (the point whose 
; coordinates are the average of the coordinates of the endpoints). 
; To try your procedures, you’ll need a way to print points:
; (define (print-point p) 
;   (newline)
;   (display "(") 
;   (display (x-point p)) 
;   (display ",")
;   (display (y-point p))
;   (display ")"))

; Points
(define (make-point x y) (cons x y))
(define (x-point point) (car point))
(define (y-point point) (cdr point))

; Segments
(define (make-segment point1 point2) (cons point1 point2))
(define (start-segment segment) (car segment))
(define (end-segment segment) (cdr segment))

; Midpoint
(define (midpoint-segment segment)
    (make-point 
        (/ (+ (x-point (start-segment segment)) 
              (x-point (end-segment segment))) 
           2) 
        (/ (+ (y-point (start-segment segment)) 
            (y-point (end-segment segment))) 
           2)))

(define (print-point p) 
    (newline)
    (display "(") 
    (display (x-point p)) 
    (display ",")
    (display (y-point p))
    (display ")"))

; Test
(define point1 (make-point 1 2))
(define point2 (make-point 2 5))
(define segment1 (make-segment point1 point2))
(define midpoint (midpoint-segment segment1))
(print-point midpoint) ;(3/2,7/2)
