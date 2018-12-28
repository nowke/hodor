; Fill in the missing expressions to complete the following definitions of some basic
; list-manipulation operations as accumulations:
;
; (define (map p sequence)
;   (accumulate (lambda (x y) <??>) nil sequence))
; (define (append seq1 seq1)
;   (accumulate cons <??> <??>))
; (define (length sequence)
;   (accumulate <??> 0 sequence))

(define nil '())
(define (accumulate op initial sequence)
    (if (null? sequence) 
        initial
        (op (car sequence) (accumulate op initial (cdr sequence)))))

(define (map p sequence)
    (accumulate 
        (lambda (x y) (cons (p x) y))
        nil
        sequence))
(map square (list 1 2 3 4)) ; (1 4 9 16)

(define (append seq1 seq2)
    (accumulate cons seq2 seq1))
(append (list 1 2 3 4) (list 5 6 7 8)) ; (1 2 3 4 5 6 7 8)

(define (length sequence)
    (accumulate (lambda (x y) (+ 1 y)) 0 sequence))
(length (list 1 2 3 5)) ;Value: 4