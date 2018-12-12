; The following pattern of numbers is called Pascal’s triangle.
;        1
;      1   1
;     1  2   1
;   1   3   3  1
;  1   4  6   4  1
;
; The numbers at the edge of the triangle are all 1, 
; and each number inside the triangle is the sum of 
; the two numbers above it. 
; Write a procedure that computes elements of Pascal’s triangle 
; by means of a recursive process

; Answer
; P(row, col) => 
;    if row = col or col = 1 => 1
;    else =>P(row - 1, col - 1) + P(row - 1, col)
;
(define (pascal row col)
    (cond ((or (= row col) (= col 1)) 1)
          (else (+ (pascal (- row 1) (- col 1)) (pascal (- row 1) col))))
)

(pascal 1 1) ; 1
(pascal 2 1) ; 1
(pascal 2 2) ; 1
(pascal 3 2) ; 2
(pascal 4 2) ; 3
(pascal 5 2) ; 4
(pascal 5 3) ; 6