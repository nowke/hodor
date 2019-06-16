; Alyssa’s program is incomplete because she has not specified 
; the implementation of the interval abstraction. Here is a 
; definition of the interval constructor:

(define (make-interval a b) (cons a b))

; Define selectors upper-bound and lower-bound to complete the implementation.

(define (add-interval x y)
    (make-interval (+ (lower-bound x) (lower-bound y))
                   (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
    (let ((p1 (* (lower-bound x) (lower-bound y)))
          (p2 (* (lower-bound x) (upper-bound y)))
          (p3 (* (upper-bound x) (lower-bound y)))
          (p4 (* (upper-bound x) (upper-bound y))))
     (make-interval (min p1 p2 p3 p4)
                    (max p1 p2 p3 p4))))

(define (div-interval x y) 
    (mul-interval
        x
        (make-interval (/ 1.0 (upper-bound y))
                       (/ 1.0 (lower-bound y)))))

(define (lower-bound z) (car z))
(define (upper-bound z) (cdr z))

(define (print-interval z)
    (newline)
    (display "[")
    (display (lower-bound z))
    (display ", ")
    (display (upper-bound z))
    (display "]"))

; Testing
; (i) Testing addition
;   [1.5, 1.8] + [5.6, 5.75] => [7.1, 7.55]
(define interval1 (make-interval 1.5 1.8))
(define interval2 (make-interval 5.6 5.75))
(define interval3 (add-interval interval1 interval2))
(print-interval interval3) ; [7.1, 7.55]

; (ii) Testing multiplication
;   [1.5, 1.8] * [5.6, 5.75] => [8.4, 10.35]
(define interval4 (mul-interval interval1 interval2))
(print-interval interval4) ; [8.399999999999999, 10.35]

; (iii) Testing division
;   [1.5, 1.8] / [5.6, 5.75] => [1.5, 1.8] * [0.1739130435, 0.1785714286]
;                            => [0.26, 0.32]
(define interval5 (div-interval interval1 interval2))
(print-interval interval5) ; [.2608695652173913, .32142857142857145]

; (iv) Testing negative
(print-interval (add-interval 
                    (make-interval -1.5 -1.2) 
                    (make-interval -1 2))) ; [-2.5, .8]
(print-interval (mul-interval 
                    (make-interval -1.5 -1.2) 
                    (make-interval -1 2))) ; [-3., 1.5]
(print-interval (div-interval 
                    (make-interval -1.5 -1.2) 
                    (make-interval -1 2))) ; [-.75, 1.5]