; Define the procedure up-split used by corner- split. It is similar to right-split, except that
; it switches the roles of below and beside.
#lang racket
(require (planet "sicp.ss" ("soegaard" "sicp.plt" 2 1)))
(define wave einstein) ; Make compatible with the text book

(define (up-split painter n)
    (if (= n 0)
        painter
        (let ((smaller (up-split painter (- n 1))))
           (below painter (beside smaller smaller)))))

; Testing
(define us1 (up-split wave 3))
(paint us1)