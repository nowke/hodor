; Apply scaling operation
(define (scale-list items factor)
    (if (null? items)
        items
        (cons (* (car items) factor)
              (scale-list (cdr items) factor))))

(scale-list (list 1 2 3 4 5) 10) ; (10 20 30 40 50)

; Abstracting the higher level idea `map`
(define (map proc items)
    (if (null? items)
        items
        (cons (proc (car items))
              (map proc (cdr items)))))

(map abs (list -10 2.5 -11.6 17)) ; (10 2.5 11.6 17)
(map square (list 4 5 6 10)) ; (16 25 36 100)
(map (lambda (x) (+ x 1)) (list 1 4 7 9)) ; (2 5 8 10)

; scale-list in terms of map
(define (scale-list items factor)
    (map (lambda (x) (* x factor)) items))
    
(scale-list (list 1 2 3 4 5) 5) ; (5 10 15 20 25)