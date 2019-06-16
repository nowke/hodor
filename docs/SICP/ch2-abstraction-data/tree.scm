; Defining hierarchical strucutre
(define x (cons (list 1 2) (list 3 4)))
(length x) ;Value: 3 (length is defined in scheme)

; Implement `count-leaves` procedure
(define (count-leaves x)
    (cond ((null? x) 0)
          ((not (pair? x)) 1)
          (else (+ (count-leaves (car x))
                   (count-leaves (cdr x))))))

(count-leaves x) ;Value: 4
(count-leaves 
    (cons (cons (list 1 2) 3) 
          (cons (list 7 8) (list 4 6)))) ;Value: 7