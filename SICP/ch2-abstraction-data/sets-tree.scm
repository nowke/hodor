; Sets as trees
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))

(define (make-tree entry left right)
    (list entry left right))

; element-of-set? operation
(define (element-of-set? x set)
    (cond ((null? set) #f)
          ((= x (entry set)) #t)
          ((< x (entry set))
            (element-of-set? x (left-branch set)))
          ((> x (entry set))
            (element-of-set? x (right-branch set)))))

; Testing `element-of-set?`
(define set1 (list 7 (list 3 (list 1 '() '()) (list 5 '() '())) 
                     (list 9 '() (list 11 '() '()))))

(entry set1) ;Value: 7
(left-branch set1) ;(3 (1) (5))
(right-branch set1) ;(9 () (11))

(element-of-set? 7 set1) ;#t
(element-of-set? 1 set1) ;#t
(element-of-set? 11 set1) ;#t
(element-of-set? 12 set1) ;#f

; Adjoin operation
(define (adjoin-set x set)
    (cond ((null? set) (make-tree x '() '()))
          ((= x (entry set)) set)
          ((< x (entry set))
            (make-tree (entry set)
                       (adjoin-set x (left-branch set))
                       (right-branch set)))
          ((> x (entry set))
            (make-tree (entry set)
                       (left-branch set)
                       (adjoin-set x (right-branch set))))))

; Test adjoin
(adjoin-set 8 set1)
; (7 (3 (1 () ()) (5 () ())) (9 (8 () ()) (11 () ())))