; Exercise 1.03
;
; Define a procedure that takes three numbers as arguments 
; and returns the sum of the squares of the two larger numbers
; 
;
; Tests:
; (sum-of-squares-larger 1 2 3) ; 13
; (sum-of-squares-larger 3 2 3) ; 18
; (sum-of-squares-larger 3 2 2) ; 13
; (sum-of-squares-larger 3 3 3) ; 18 
;
(define (square x) (* x x))
(define (sum-of-square x y) (+ (square x) (square y)))

(define (sum-of-squares-larger a b c)
   (cond 
      ((and (>= a b) (>= b c)) (sum-of-square a b))
      ((and (>= a b) (>= c b)) (sum-of-square a c))
      ((and (>= b a) (>= c a)) (sum-of-square b c))
   )
)
