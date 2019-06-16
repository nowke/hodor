; A binary mobile consists of two branches, a left branch and a right branch. 
; Each branch is a rod of a certain length, from which hangs either a weight 
; or another binary mobile. We can represent a binary mobile using compound 
; data by constructing it from two branches (for example, using list):
(define (make-mobile left right) 
    (list left right))

; A branch is constructed from a length (which must be a number) together 
; with a structure, which may be either a number (representing a simple weight) 
; or another mobile:
(define (make-branch length structure) 
    (list length structure))

; (a) Write the corresponding selectors `left-branch` and `right-branch`, which 
;     return the branches of a mobile, and `branch-length` and `branch-structure`, 
;     which return the components of a branch.
;  Ans
(define (left-branch mobile) (car mobile))
(define (right-branch mobile) (car (cdr mobile))) ; (cdr x) => (right), (car (right)) => right
(define (branch-length branch) (car branch))
(define (branch-structure branch) (car (cdr branch)))

; (b) Using your selectors, define a procedure `total-weight` that returns the 
;     total weight of a mobile.
;  Ans
(define (total-weight mobile)
    (cond ((null? mobile) 0)
          ((not (pair? mobile)) mobile)
          (else (+ (total-weight (branch-structure (left-branch mobile)))
                   (total-weight (branch-structure (right-branch mobile)))))))

; Testing
(define m1 (make-mobile 
            (make-branch 5 20)
            (make-branch 6 25)))
(left-branch m1) ; (5 20)
(right-branch m1) ; (6 25)
(branch-length (left-branch m1)) ;Value: 5
(branch-structure (left-branch m1)) ; 20
(branch-length (right-branch m1)) ;Value: 6
(branch-structure (right-branch m1)) ;Value: 25

(total-weight m1) ;Value: 45 (20 + 25)

(define m2 (make-mobile
            (make-branch 5 
                         (make-mobile (make-branch 8 22) 
                                      (make-branch 9 10)))
            (make-branch 6 25)))
(total-weight m2) ;Value: 57 (22 + 10 +25)

; (c) A mobile is said to be balanced if the torque applied by its top-left 
;     branch is equal to that applied by its top-right  branch (that is, if 
;     the length of the left rod multiplied by the weight hanging from that 
;     rod is equal to the corresponding product for the right side) and if 
;     each of the submobiles hanging off its branches is balanced. Design a 
;     predicate that tests whether a binary mobile is balanced.

; Torque of a branch = (length) x (total weight of it's structure)
(define (torque branch)
    (* (branch-length branch) 
        (total-weight (branch-structure branch))))

(define (is-balanced? mobile)
    (if (not (pair? mobile))
        #t  ; Return true for a plain number
        (and (= (torque (left-branch mobile)) (torque (right-branch mobile)))
             (is-balanced? (branch-structure (left-branch mobile)))
             (is-balanced? (branch-structure (right-branch mobile)))))
)

; Testing
(define b1 
    (make-mobile 
        (make-branch 5 10)
        (make-branch 25 2)))
(is-balanced? b1) ;Value: #t (5x10 = 25x2 = 50)

(define b2
    (make-mobile
        (make-branch 
            5 
            (make-mobile
                (make-branch 1 8)
                (make-branch 4 2)))
        (make-branch 25 2)))
; Here, 
;   Torque of right-b2 = 25 x 2 = 50
;   Torque of left-b2 = 5 * (weight of structure(left-b2))
;                     = 5 * (8 + 2)
;                     = 50
;   Also, inside structure(left-b2)
;       Torques - (1 x 8) = (4 x 2) = 8
;  Hence, mobile is balanced
(is-balanced? b2) ;Value: #t 

; (d) Suppose we change the representation of mobiles so that the constructors are
(define (make-mobile left right) 
    (cons left right)) 
(define (make-branch length structure)
    (cons length structure))
;     How much do you need to change your programs to convert to the new 
;     representation?

; Ans: We need to slightly change the definition of the following
(define (left-branch mobile) (car mobile))
(define (right-branch mobile) (cdr mobile))
(define (branch-length branch) (car branch))
(define (branch-structure branch) (cdr branch))

; Rest will work as it is,
; Testing
; (Redefine `total-weight`)
(define (total-weight mobile)
    (cond ((null? mobile) 0)
          ((not (pair? mobile)) mobile)
          (else (+ (total-weight (branch-structure (left-branch mobile)))
                   (total-weight (branch-structure (right-branch mobile)))))))
(define m3 (make-mobile 
            (make-branch 5 20)
            (make-branch 6 25)))
(total-weight m3) ;Value: 45