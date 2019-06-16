; Implement the lookup procedure for the case where the set of records is 
; structured as a binary tree, ordered by the numerical values of the keys.
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))

(define (key record) (car record))

(define (lookup given-key set-of-records)
    (cond ((null? set-of-records) #f)
          ((= given-key (key (entry set-of-records)))
           (entry set-of-records))
          ((< given-key (key (entry set-of-records)))
           (lookup given-key (left-branch set-of-records)))
          ((> given-key (key (entry set-of-records)))
           (lookup given-key (right-branch set-of-records)))))

; Testing
(define tree1 (list (cons 7 "Emp7") (list (cons 3 "Emp3") (list (cons 1 "Emp1") '() '()) 
    (list (cons 5 "Emp5") '() '())) 
    (list (cons 9 "Emp9") '() (list (cons 11 "Emp11") '() '()))))

(lookup 7 tree1) ;(7 . "Emp7")
(lookup 12 tree1) ;#f