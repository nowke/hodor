; Suppose we represent vectors v = (v_i) as sequences of numbers, and matrices 
; m = (m_{ij}) as sequences of vectors (the rows of the matrix). For example, the 
; matrix
;  1 2 3 4
;  4 5 6 6
;  6 7 8 9
; is represented as the sequence ((1 2 3 4) (4 5 6 6) (6 7 8 9)). With this 
; representation, we can use sequence operations to concisely express the basic 
; matrix and vector operations. These operations (which are described in any book 
; on matrix algebra) are the following:
; (dot-product v w)     returns the sum Σiviwi;
; (matrix-*-vector m v) returns the vector t, whereti =Σjmijvj;
; (matrix-*-matrix m n) returns the matrix p, wherepij =Σkmiknkj;
; (transpose m)         returns the matrix n, wherenij =mji.
; 
; We can define the dot product as,
; (define (dot-product v w)
;   (accumulate + 0 (map * v w)))
; Fill in the missing expressions in the following procedures for computing 
; the other matrix operations. (The procedure accumulate-n is defined in 
; Exercise 2.36.)
; (define (matrix-*-vector m v) (map ⟨??⟩ m))
; (define (transpose mat) (accumulate-n ⟨??⟩ ⟨??⟩ mat))
; (define (matrix-*-matrix m n) (let ((cols (transpose n)))
;     (map ⟨??⟩ m)))

(define nil '())
(define (accumulate op initial sequence)
    (if (null? sequence) 
        initial
        (op (car sequence) (accumulate op initial (cdr sequence)))))
(define (accumulate-n op initial seqs)
    (if (null? (car seqs))
        nil
        (cons (accumulate op initial (map car seqs))
              (accumulate-n op initial (map cdr seqs)))))

; Dot product 
(define (dot-product v w)
    (accumulate + 0 (map * v w)))

; Matrix x Vector
(define (matrix-*-vector m v)
    (map (lambda (x) (dot-product x v)) m))

; Transpose
(define (transpose mat)
    (accumulate-n cons nil mat))

; Matrix x Matrix
(define (matrix-*-matrix m n)
    (let ((cols (transpose n)))
        (map (lambda (row) 
                (matrix-*-vector cols row))
             m)))

; Testing
(define v1 (list 1 2 3 4))
(define v2 (list 2 3 4 5))

; Dot product v1 \cdot v2 = 1x2 + 2x3 + 3x4 + 4x5 = 40
(dot-product v1 v2) ;Value: 40

(define m1 (list (list 1 3 4) (list 1 0 1) (list 2 1 4)))
(define v3 (list 1 2 3))

; m1 x v3
(matrix-*-vector m1 v3) ;(19 4 16)

(define m2 (list (list 0 0 1) (list 2 3 5) (list 1 2 9)))
(matrix-*-matrix m1 m2) ; ((10 17 52) (1 2 10) (6 11 43))

(transpose m1) ; ((1 1 2) (3 0 1) (4 1 4))