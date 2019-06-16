; Define the transformation flip-horiz, which flips painters horizontally, 
; and transformations that rotate painters counterclockwise by 180 degrees 
; and 270 degrees.
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

; (i) Flip horizontally
(define (flip-horiz painter)
  (transform-painter painter
                     (make-vect 1 0)
                     (make-vect 0 0)
                     (make-vect 1 1)))

; Test horizontal flip
(define f1-list (list (make-segment (make-vect 0.3 0.3) (make-vect 0.3 0.7))
                      (make-segment (make-vect 0.3 0.7) (make-vect 0.7 0.5))
                      (make-segment (make-vect 0.7 0.5) (make-vect 0.3 0.3))))
(define f1 (segments->painter f1-list))
(display "\nFrame f1\n")
(paint f1)

(display "\nFrame flipped horizontally f1\n")
(paint (flip-horiz f1))

; (ii) Counterclockwise 180deg
(define (rotate180 painter)
  (transform-painter painter
                     (make-vect 1 1)
                     (make-vect 0 1)
                     (make-vect 1 0)))
(display "\nFrame rotate counter clockwise 180deg f1\n")
(paint (rotate180 f1))

; (iii) Counterclockwise 270deg
(define (rotate270 painter)
  (transform-painter painter
                     (make-vect 0 1)
                     (make-vect 0 0)
                     (make-vect 1 1)))
(display "\nFrame rotate counter clockwise 270deg f1\n")
(paint (rotate270 f1))