; Each of the following two procedures converts a binary tree to a list.
(define (tree->list-1 tree) 
    (if (null? tree)
        '()
        (append (tree->list-1 (left-branch tree))
                (cons (entry tree)
                      (tree->list-1
                        (right-branch tree))))))
(define (tree->list-2 tree)
    (define (copy-to-list tree result-list)
        (if (null? tree)
            result-list
            (copy-to-list (left-branch tree)
                          (cons (entry tree)
                                (copy-to-list
                                    (right-branch tree)
                                    result-list)))))
    (copy-to-list tree '()))

; (a) Do the two procedures produce the same result for
;     every tree? If not, how do the results differ? What lists
;     do the two procedures produce for the trees in Figure 2.16?

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))

(define (make-tree entry left right)
    (list entry left right))

; Test for all 3 trees in Figure 2.16
(define tree1 (list 7 (list 3 (list 1 '() '()) (list 5 '() '())) 
    (list 9 '() (list 11 '() '()))))
(define tree2 
    (list 3 (list 1 '() '()) 
            (list 7 (list 5 '() '())
                    (list 9 '() (list 11 '() '())))))
(define tree3
    (list 5 (list 3 (list 1 '() '()) '())
            (list 9 (list 7 '() '())
                    (list 11 '() '()))))

(tree->list-1 tree1) ;(1 3 5 7 9 11)
(tree->list-2 tree1) ;(1 3 5 7 9 11)

(tree->list-1 tree2) ;(1 3 5 7 9 11)
(tree->list-2 tree2) ;(1 3 5 7 9 11)

(tree->list-1 tree3) ;(1 3 5 7 9 11)
(tree->list-2 tree3) ;(1 3 5 7 9 11)

; Ans: Both the procedures produce the same results for every tree
; For all 3 figures in Fig 2.16, list is (1 3 5 7 9 11)

; (b) Do the two procedures have the same order of growth in the number of 
;     steps required to convert a balanced tree with n elements to a list? 
;     If not, which one grows more slowly?

; Ans
; tree->list-1 takes O(n log n) time
; tree->list-2 takes O(n) time