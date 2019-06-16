; Implement a representation for rectangles in a plane. 
; (Hint: You may want to make use of Exercise 2.2.) In 
; terms of your constructors and selectors, create 
; procedures that compute the perimeter and the area of 
; a given rectangle. Now implement a different representation 
; for rectangles. Can you design your system with suitable 
; abstraction barriers, so that the same perimeter and area 
; procedures will work using either representation?

; Points
(define (make-point x y) (cons x y))
(define (x-point point) (car point))
(define (y-point point) (cdr point))

; Rectangle
; (node1) --------------
;         |             |
;         |             |
;         |             |
;         --------------- (node2)
(define (make-rect node1 node2) (cons node1 node2))
(define (rect-node1 rect) (car rect))
(define (rect-node2 rect) (cdr rect))

; Procedure for calculating width (x2 - x1)
(define (get-rect-width rect)
    (abs (- (x-point (rect-node1 rect)) 
         (x-point (rect-node2 rect)))))

; Procedure for calculating height (y2 - y1)
(define (get-rect-height rect)
    (abs (- (y-point (rect-node1 rect)) 
            (y-point (rect-node2 rect)))))
; Perimeter
(define (perimeter rect)
    (* 2 (+ (get-rect-width rect) (get-rect-height rect))))

; Area
(define (area rect)
    (* (get-rect-width rect) (get-rect-height rect)))

; Testing
(define myrect (make-rect (make-point 2 4) (make-point 6 9)))
(perimeter myrect) ;Value: 18
(area myrect) ;Value: 20

; -------------------------------

; Let's define another simplest representation of rectangle
; using just plain numbers (width and height)
;
;           width
;  ------------------------
;  |                      |
;  |                      |  height
;  |                      |
;  ------------------------

(define (make-rect-2 width height) (cons width height))
(define (get-rect-width rect) (car rect))
(define (get-rect-height rect) (cdr rect))

; Testing
(define myrect2 (make-rect-2 4 10))
(perimeter myrect2) ;Value: 28
(area myrect2) ;Value: 40

; We defined another representation of rectangle and did not
; have to change the implementation of `perimeter` and `area`