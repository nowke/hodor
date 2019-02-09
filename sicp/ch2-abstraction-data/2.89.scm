; Define procedures that implement the `term-list` representation described above as 
; appropriate for dense polynomials.

; Representation of 2x^3 + 5x + 7 will be,
; (2 0 5 7)

; We essentially need to change `adjoin-term` to store internally
; in the above representation while preserving the same data structure as before

(define (adjoin-term term term-list)
    (cond ((=zero? term) term-list)
          ((= (order term) (length term-list))
           (cons (coeff term) term-list))
          (else (adjoin-term term
                             (cons 0 term-list)))))

; Attaching 4x^4 to (2x^3 + 5x + 7) will be,
; (adjoin-term (4 3) (2 0 5 7))
;   => (3 2 0 5 7)

; Attaching 3x^5 to (2x^3 + 5x + 7) will be,
; (adjoin-term (5 3) (2 0 5 7))
;   => (adjoin-term (5 3) (0 2 0 5 7))
;   => (3 0 2 0 5 7)