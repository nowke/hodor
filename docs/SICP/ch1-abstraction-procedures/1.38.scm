;  In 1737, the Swiss mathematician Leonhard Euler published 
; a memoir De Fractionibus Continuis, which included a 
; continued fraction expansion for e−2, where e is the base 
; of the natural logarithms. In this fraction, the Ni are all 1, 
; and the Di are successively 1, 2, 1, 1, 4, 1, 1, 6, 1, 1, 8, . . .. 
; Write a program that uses your cont-frac procedure from 
; Exercise 1.37 to approximate e, based on Euler’s expansion

; To calculate Di, we can observe 2,5,8..th terms are 2,4,6,8...
; i.e. if remainder(i/3) = 2 -> Di = 3(i + 1)/2, else Di = 1

(define (cont-frac n d k)
    (define (iter k res)
        (if (= k 0)
            res
            (iter (- k 1)
                  (/ (n k) (+ (d k) res)))))
    (iter k 0))

(+ (cont-frac (lambda (x) 1)
              (lambda (x) 
                    (if (= (remainder x 3) 2)
                        (/ (+ x 1) 1.5) 
                        1))
              200) 2) ;Value: 2.7182818284590455