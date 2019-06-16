; right-split and up-split can be expressed as instances of a general splitting operation. Define a procedure
; split with the property that evaluating
; (define right-split (split beside below))
; (define up-split (split below beside))
; produces procedures right-split and up-split with the same behaviors as the ones already defined.
#lang racket
(require (planet "sicp.ss" ("soegaard" "sicp.plt" 2 1)))
(define wave einstein) ; Make compatible with the text book

(define (split s1 s2)
  (define (splitter painter n)
    (if (= n 0)
        painter
        (let ((smaller (splitter painter (- n 1))))
          (s1 painter (s2 smaller smaller)))))
  (lambda (painter n) (splitter painter n)))

(define right-split (split beside below))
(define up-split (split below beside))

(define rs3 (right-split wave 3))
(display "Right split\n")
(paint rs3)

(define us3 (up-split wave 3))
(display "\nUp split\n")
(paint us3)