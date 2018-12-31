; Use segments->painter to define the following primitive painters
; a. The painter that draws the outline of the designated frame.
; b. The painter that draws an “X” byconnecting opposite corners of the frame.
; c. The painter that draws a diamond shape by connecting the midpoints of the sides of the frame.
; d. The wave painter.
#lang racket
(require (planet "sicp.ss" ("soegaard" "sicp.plt" 2 1)))

; NOTE: Procedure `segments->painter` is already preset in "soegaard" package

(define (make-vect x y) (cons x y))
(define (xcor-vect v) (car v))
(define (ycor-vect v) (cdr v))

(define (make-segment v1 v2) (cons v1 v2))
(define (start-segment segment) (car segment))
(define (end-segment segment) (cdr segment))

; (a) Outline of a frame (unit square)
; For some reason, giving "1.0" does not render a few edges, hence using 0.99 instead
(define outline-list
  (list
   (make-segment (make-vect 0 0) (make-vect 0 0.99))
   (make-segment (make-vect 0 0) (make-vect 0.99 0))
   (make-segment (make-vect 0.99 0) (make-vect 0.99 0.99))
   (make-segment (make-vect 0 0.99) (make-vect 0.99 0.99))))
(define outline (segments->painter outline-list))
(display "Outline of frame\n")
(paint outline)

; (b) "X" mark
(define x-list
  (list
   (make-segment (make-vect 0 0) (make-vect 1 1))
   (make-segment (make-vect 0 1) (make-vect 1 0))))
(define x-mark (segments->painter x-list))
(display "\nX-mark\n")
(paint x-mark)

; (c) Diamond
(define diamond-list
  (list
   (make-segment (make-vect 0   0.5) (make-vect 0.5   0))
   (make-segment (make-vect 0   0.5) (make-vect 0.5   1))
   (make-segment (make-vect 0.5   1) (make-vect 1   0.5))
   (make-segment (make-vect 1   0.5) (make-vect 0.5   0))))
(define diamond (segments->painter diamond-list))
(display "\nDiamond\n")
(paint diamond)

; (d) Wave shape
; Suggestion: Draw the shape on 0-100 rough scale and figure out line segments :)
; Also, excuse me for the wierd shape

(define wave-list
  (list
   (make-segment (make-vect  0.01 0.8) (make-vect  0.1 0.56))
   (make-segment (make-vect  0.1 0.56) (make-vect  0.2 0.6))
   (make-segment (make-vect  0.2 0.6) (make-vect 0.3 0.58))
   (make-segment (make-vect  0.3 0.58) (make-vect  0.2 0.75))
   (make-segment (make-vect  0.2 0.75) (make-vect  0.4 1))
   (make-segment (make-vect  0.6 1) (make-vect  0.8 0.75))
   (make-segment (make-vect  0.8 0.75) (make-vect  0.7 0.58))
   (make-segment (make-vect  0.7 0.58) (make-vect  0.8 0.6))
   (make-segment (make-vect  0.8 0.6) (make-vect  1 0.4))
   (make-segment (make-vect  0 0.61) (make-vect  0.1 0.35))
   (make-segment (make-vect  0.1 0.35) (make-vect  0.2 0.5))
   (make-segment (make-vect  0.2 0.5) (make-vect  0.3 0.35))
   (make-segment (make-vect  0.3 0.35) (make-vect  0.2 0))
   (make-segment (make-vect  0.3 0) (make-vect 0.5 0.25))
   (make-segment (make-vect  0.5 0.25) (make-vect 0.65 0))
   (make-segment (make-vect  0.75 0) (make-vect  0.6 0.4))
   (make-segment (make-vect  0.6 0.4) (make-vect  1 0.2))))
(define wave (segments->painter wave-list))
(display "\nWave\n")
(paint wave)