#lang racket
(require (planet "sicp.ss" ("soegaard" "sicp.plt" 2 1)))

; NOTE: "frame-coord-map", "make-vect", "make-frame", "make-segment", "segments->painter"
; are defined in "soegaard"

; Define helpers
(define (xcor-vect v) (car v))
(define (ycor-vect v) (cdr v))
(define (sub-vect v1 v2)
    (make-vect 
        (- (xcor-vect v1) (xcor-vect v2))
        (- (ycor-vect v1) (ycor-vect v2))))

; Transform painter
(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter (make-frame
                   new-origin
                   (sub-vect (m corner1) new-origin)
                   (sub-vect (m corner2) new-origin)))))))

; Flip-vertically
(define (flip-vert painter)
  (transform-painter painter
                     (make-vect 0 1)   ; new origin
                     (make-vect 1 1)   ; new end of edge1
                     (make-vect 0 0))) ; new end of edge2

; Test vertical flip
(define f1-list (list (make-segment (make-vect 0.3 0.3) (make-vect 0.5 0.7))
                      (make-segment (make-vect 0.5 0.7) (make-vect 0.8 0.3))))
(define f1 (segments->painter f1-list))
(display "\nFrame f1\n")
(paint f1)

(display "\nFrame Flipped f1\n")
(paint (flip-vert f1))

; Shrink image to upper-right-corner
(define (shrink-to-upper-right painter)
  (transform-painter
   painter (make-vect 0.5 0.5)
   (make-vect 1.0 0.5) (make-vect 0.5 1.0)))

(display "\nFrame Shrink upper right corner f1\n")
(paint (shrink-to-upper-right f1))

; Rotate 90 degrees counterclockwise
(define (rotate90 painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))

(display "\nFrame Rotate 90deg f1\n")
(paint (rotate90 f1))

; Squash inwards
(define (squash-inwards painter)
  (transform-painter painter
                     (make-vect 0.0 0.0)
                     (make-vect 0.65 0.35)
                     (make-vect 0.35 0.65)))

(display "\nFrame Squash inwards f1\n")
(paint (squash-inwards f1))

; Beside operation by transformation
(define (beside painter1 painter2)
  (let ((split-point (make-vect 0.5 0.0)))
    (let ((paint-left (transform-painter
            painter1
            (make-vect 0.0 0.0)
            split-point
            (make-vect 0.0 1.0)))
          (paint-right
           (transform-painter
            painter2
            split-point
            (make-vect 1.0 0.0)
            (make-vect 0.5 1.0))))
      (lambda (frame) (paint-left frame) (paint-right frame)))))

(display "\nBeside f1 f1\n")
(paint (beside f1 f1))