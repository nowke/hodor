; Implement the union-set operation for the unordered-list representation of sets.

(define (union-set set1 set2)
    (cond ((and (null? set1) (not (null? set2))) set2)
          ((and (null? set2) (not (null? set1))) set1)
          ((element-of-set? (car set1) set2)
           (union-set (cdr set1) set2))
          (else (cons (car set1)
                      (union-set (cdr set1) set2)))))

(define (element-of-set? x set)
    (cond ((null? set) false)
          ((equal? x (car set)) #t)
          (else (element-of-set? x (cdr set)))))

(union-set (list 1 2 3 4) (list 2 3 4 5 6))
;Value 2: (1 2 3 4 5 6)