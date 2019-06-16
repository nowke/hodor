#lang racket
(require (planet "sicp.ss" ("soegaard" "sicp.plt" 2 1)))
(define wave einstein) ; Make compatible with the text book

(define wave2 (beside wave (flip-vert wave)))
(define wave4 (below wave2 wave2))

(display "beside wave \n")
(paint wave2)

(display "\nbeside below wave \n")
(paint wave4)

; Creating abstraction for `wave4`
(define (flipped-pairs painter)
  (let ((painter2 (beside painter (flip-vert painter))))
    (below painter2 painter2)))

(define wave5 (flipped-pairs wave))
(display "\nflipped-pairs\n")
(paint wave5)

; Right split procedure
(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))

(define rs4 (right-split wave 4))
(display "\nRight split 4\n")
(paint rs4)

; Up split procedure
(define (up-split painter n)
    (if (= n 0)
        painter
        (let ((smaller (up-split painter (- n 1))))
           (below painter (beside smaller smaller)))))

; Defining `corner-split` procedure
(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))

(define cs3 (corner-split wave 3))
(display "\nCorner split 3\n")
(paint cs3)

; Define `square-limit` pattern
(define (square-limit painter n)
  (let ((quarter (corner-split painter n)))
    (let ((half (beside (flip-horiz quarter) quarter)))
      (below (flip-vert half) half))))

(define sl3 (square-limit wave 3))
(display "\nSquare limit pattern 3\n")
(paint sl3)