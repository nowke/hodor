; A two-dimensional vector v running from the origin to a point can be represented 
; as a pair consisting of an x-coordinate and a y-coordinate. Implement a data 
; abstraction for vectors by giving a constructor make-vect and corresponding 
; selectors xcor-vect and ycor-vect. In terms of your selectors and constructor, 
; implement procedures add-vect, sub-vect, and scale-vect that perform the 
; operations vector addition, vector subtraction, and multiplying a vector by a 
; scalar:
; (x1,y1)+(x2,y2)=(x1 +x2,y1 +y2), 
; (x1,y1)−(x2,y2)=(x1 −x2,y1 −y2),
; s · (x , y) = (sx , sy).

(define (make-vect x y) (cons x y))
(define (xcor-vect v) (car v))
(define (ycor-vect v) (cdr v))

; Vector operaitons
(define (add-vect v1 v2)
    (make-vect 
        (+ (xcor-vect v1) (xcor-vect v2))
        (+ (ycor-vect v1) (ycor-vect v2))))
(define (sub-vect v1 v2)
    (make-vect 
        (- (xcor-vect v1) (xcor-vect v2))
        (- (ycor-vect v1) (ycor-vect v2))))
(define (scale-vect s vect)
    (make-vect (* s (xcor-vect vect)) 
               (* s (ycor-vect vect))))

; Testing
(add-vect (make-vect 1 3) (make-vect 2 8)) ;(3 . 11)
(sub-vect (make-vect 2 5) (make-vect 0 3)) ;(2 . 2)
(scale-vect 4 (make-vect 3 5)) ;(12 . 20)