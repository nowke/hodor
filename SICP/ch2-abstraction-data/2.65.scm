; Use the results of Exercise 2.63 and Exercise 2.64 to give Θ(n) implementations 
; of `union-set` and `intersection-set` for sets implemented as (balanced) binary 
; trees

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))

(define (make-tree entry left right)
    (list entry left right))

(define (tree->list tree)
    (define (copy-to-list tree result-list)
        (if (null? tree)
            result-list
            (copy-to-list (left-branch tree)
                          (cons (entry tree)
                                (copy-to-list
                                    (right-branch tree)
                                    result-list)))))
    (copy-to-list tree '()))

(define (list->tree elements)
    (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result
                (partial-tree elts left-size)))
          (let ((left-tree (car left-result)) 
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts)) 
                  (right-result
                    (partial-tree
                      (cdr non-left-elts)
                      right-size)))
                (let ((right-tree (car right-result)) 
                      (remaining-elts
                        (cdr right-result)))
                    (cons (make-tree this-entry
                                     left-tree
                                     right-tree)
                          remaining-elts))))))))

; Union using ordered list
(define (union-set-list set1 set2)
    (cond ((null? set1) set2)
          ((null? set2) set1)
          (else
            (let ((x1 (car set1))
                  (x2 (car set2)))
                (cond ((= x1 x2)
                       (cons x1 (union-set-list (cdr set1) (cdr set2))))
                      ((> x1 x2)
                       (cons x2 (union-set-list set1 (cdr set2))))
                      (else
                        (cons x1 (union-set-list (cdr set1) set2))))))))
(define (union-set set1 set2)
    (list->tree (union-set-list
                    (tree->list set1)
                    (tree->list set2))))

; Intersection using ordered list
(define (intersection-set-list set1 set2) 
    (if (or (null? set1) (null? set2))
        '()
        (let ((x1 (car set1)) (x2 (car set2)))
          (cond ((= x1 x2)
                 (cons x1 (intersection-set-list (cdr set1)
                                            (cdr set2))))
                ((< x1 x2)
                 (intersection-set-list (cdr set1) set2))
                ((< x2 x1)
                 (intersection-set-list set1 (cdr set2)))))))
(define (intersection-set set1 set2)
    (list->tree (intersection-set-list
                    (tree->list set1)
                    (tree->list set2))))

(tree->list (union-set 
    (list->tree '(1 2 3 5 6 8 12))  
    (list->tree '(5 6 9 10 12 13 15))))
; (1 2 3 5 6 8 9 10 12 13 15)

(tree->list (intersection-set 
    (list->tree '(1 2 3 5 6 8 12))  
    (list->tree '(5 6 9 10 12 13 15))))
; (5 6 12)