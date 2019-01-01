; Make changes to the square limit of wave shown in Figure 2.9 by working at each 
; of the levels described above. In particular:
; a. Add some segments to the primitive wave painter of Exercise 2.49 (to add a 
;    smile, for example).
; b. Change the pattern constructed by corner-split (for example, by using only 
;    one copy of the up-split and right-split images instead of two).
; c. Modify the version of square-limit that uses square-of-four so as to assemble 
;    the corners in a different pattern. (For example, you might make the big Mr. 
;    Rogers look outward from each corner of the square.)
#lang racket
(require (planet "sicp.ss" ("soegaard" "sicp.plt" 2 1)))

; (a) Added a weird v shaped smile :>
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
     (make-segment (make-vect  0.6 0.4) (make-vect  1 0.2))
     (make-segment (make-vect  0.4 0.8) (make-vect  0.5 0.6))
     (make-segment (make-vect  0.5 0.6) (make-vect  0.6 0.8))))
(define wave (segments->painter wave-list))
(display "\nWave with wierd smile\n")
(paint wave)

; (b) Corner split using only 1 copy of up-split and right-split
(define (up-split painter n)
    (if (= n 0)
        painter
        (let ((smaller (up-split painter (- n 1))))
           (below painter (beside smaller smaller)))))
(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))
(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left up)
              (bottom-right right)
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))

(display "\nCorner split modifined\n")
(paint (corner-split wave 3))

; (c) Modified square-limit
(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
          (bottom (beside (bl painter) (br painter))))
      (below bottom top))))
(define (square-limit painter n)
  (let ((combine4 (square-of-four rotate270 rotate180
                                  identity rotate90)))
    (combine4 (corner-split painter n))))

(display "\nOutward square-limit\n")
(paint (square-limit wave 2))