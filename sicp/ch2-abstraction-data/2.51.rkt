; Define the below operation for painters. below takes two painters as arguments. The resulting painter, given
; a frame, draws with the first painter in the bottom of the frame and with the second painter in the top.
; Define below in two different ways — first by writing a procedure that is analogous to the beside procedure
; given above, and again in terms of beside and suitable rotation operations (from Exercise 2.50).
#lang racket
(require (planet "sicp.ss" ("soegaard" "sicp.plt" 2 1)))

; Helpers
(define (xcor-vect v) (car v))
(define (ycor-vect v) (cdr v))
(define (sub-vect v1 v2)
    (make-vect 
        (- (xcor-vect v1) (xcor-vect v2))
        (- (ycor-vect v1) (ycor-vect v2))))

(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter (make-frame
                   new-origin
                   (sub-vect (m corner1) new-origin)
                   (sub-vect (m corner2) new-origin)))))))

; (i) Below procedure analogous to `beside`
(define (below painter1 painter2)
  (let ((split-point (make-vect 0 0.5)))
    (let ((paint-bottom (transform-painter
            painter1
            (make-vect 0 0)
            (make-vect 1 0)
            split-point))
          (paint-top
           (transform-painter
            painter2
            split-point
            (make-vect 1 0.5)
            (make-vect 0 1))))
      (lambda (frame) (paint-bottom frame) (paint-top frame)))))

; Test 
(define f1-list (list (make-segment (make-vect 0.3 0.3) (make-vect 0.5 0.7))
                      (make-segment (make-vect 0.5 0.7) (make-vect 0.8 0.3))))
(define f1 (segments->painter f1-list))
(display "\nFrame 1\n")
(paint f1)

(display "\nBelow Frame1 & Frame1\n")
(paint (below f1 f1))

; (ii) Below procedure using beside and rotation
(define (below2 painter1 painter2)
  (rotate270 (beside (rotate90 painter2) (rotate90 painter1))))

; Test
(display "\nBelow Frame1 & Frame1\n")
(paint (below2 f1 f1))