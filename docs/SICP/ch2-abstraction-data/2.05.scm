; Show that we can represent pairs of nonnegative integers 
; using only numbers and arithmetic operations if we represent 
; the pair a and b as the integer that is the product 2^a * 3^b. 
; Give the corresponding definitions of the procedures cons, car, 
; and cdr.

; Answer
; -----------------
; Let's take two numbers (a,b) => (5, 8)
; If n = 2^5 * 3^8,
;  => we can retrieve "5" from n by counting the number of times
;     n/2 yeilds zero remainder
;  Similarly, we cna retrieve "8" from n by counting the number of
; times n/3 yields zero remainder

; Let's define a procedure to extact a or b (say x) from n
(define (extract-exp n x)
    (define (iter n count)
        (if (= (remainder n x) 0)
            (iter (/ n x) (+ count 1))
            count))
    (iter n 0))

; Let's write corresponding `cons`, `car` and `cdr`
(define (cons a b)
    (* (expt 2 a) (expt 3 b)))

(define (car x) (extract-exp x 2))
(define (cdr x) (extract-exp x 3))

; Testing
(define pair1 (cons 5 8))
(car pair1) ;Value: 5
(cdr pair1) ;Value: 8