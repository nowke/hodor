; Here are two possible constructors for frames:
; (define (make-frame origin edge1 edge2) (list origin edge1 edge2))
; (define (make-frame origin edge1 edge2) (cons origin (cons edge1 edge2)))
; For each constructor supply the appropriate selectors to produce an implementation for frames.

(define (make-vect x y) (cons x y))
(define (xcor-vect v) (car v))
(define (ycor-vect v) (cdr v))

; Implementation 1
(define (make-frame origin edge1 edge2) 
    (list origin edge1 edge2))
(define (frame-origin frame) (car frame))
(define (frame-edge1 frame) (cadr frame))
(define (frame-edge2 frame) (cadr (cdr frame)))

(define f1 (make-frame 
            (make-vect 1 1) 
            (make-vect 2 3)
            (make-vect 7 8))) ; ((1 . 1) (2 . 3) (7 . 8))
(frame-origin f1) ; (1 . 1)
(frame-edge1 f1) ; (2 . 3)
(frame-edge2 f1) ; (7 . 8)

; Implementation 2
(define (make-frame origin edge1 edge2) 
    (cons origin (cons edge1 edge2)))
(define (frame-origin frame) (car frame))
(define (frame-edge1 frame) (cadr frame))
(define (frame-edge2 frame) (cdr (cdr frame)))

(define f2 (make-frame 
            (make-vect 1 1) 
            (make-vect 2 3)
            (make-vect 7 8))); ((1 . 1) (2 . 3) 7 . 8)

(frame-origin f2) ; (1 . 1)
(frame-edge1 f2) ; (2 . 3)
(frame-edge2 f2) ; (7 . 8)