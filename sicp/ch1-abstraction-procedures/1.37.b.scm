; If your cont-frac procedure generates a recursive process, 
; write one that generates an iterative process. If it 
; generates an iterative process, write one that generates
; a recursive process

; Iterative version of `cont-frac`
(define (cont-frac-iter n d k)
    (define (iter k res)
        (if (< k 0)
            res
            (iter (- k 1)
                  (/ (n k) (+ (d k) res)))))
    (iter k 0))

; Testing
(cont-frac-iter (lambda (i) 1.0) (lambda (i) 1.0) 100) ;Value: .6180339887498948