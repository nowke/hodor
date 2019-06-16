; Suppose we want to handle complex numbers whose real parts, imaginary parts, 
; magnitudes, and angles can be either ordinary numbers, rational numbers, or 
; other numbers we might wish to add to the system. Describe and implement the 
; changes to the system needed to accommodate this. You will have to define 
; operations such as sine and cosine that are generic over ordinary numbers and 
; rational numbers.

; Source: [https://wizardbook.wordpress.com/2010/12/09/exercise-2-86/](https://wizardbook.wordpress.com/2010/12/09/exercise-2-86/)

; Previously, the arithmetic operators needed to convert between polar and 
; rectangular representations of complex numbers only needed to work on scheme’s 
; built in number values. Now those values can be any of the types in the number 
; system and so all the operators used in those conversions need to use generic 
; procedures.

; in the polar package
(define (real-part z)
  (mul (magnitude z) (cosine (angle z))))
 
(define (imag-part z)
  (mul (magnitude z) (sine (angle z))))
 
(define (make-from-real-imag x y) 
  (make-from-mag-ang (sq-root (add (square x) (square y)))
                     (arctan y x)))

; in the rectangular package
(define (magnitude z)
  (sq-root (add (square (real-part z))
                (square (imag-part z)))))
 
(define (angle z)
  (arctan (imag-part z) (real-part z)))
 
(define (make-from-mag-ang r a) 
  (cons (mul r (cosine a)) (mul r (sine a))))

; This means we need new top-level generic procedures:
(define (square x)    (apply-generic 'square x))
(define (sq-root x)   (apply-generic 'sq-root x))
(define (sine x)      (apply-generic 'sine x))
(define (cosine x)    (apply-generic 'cosine x))
(define (arctan x y)  (apply-generic 'arctan x y))

; These procedures need to be implemented by the real, rational and integer packages
; and to be installed into the operations table.

; in the integer package
; does it make any sense to have sine, cosine, sq-root and arctan for integer?
(put 'sq-root    '(integer)         (lambda (x)   (make-real (sqrt x))))
(put 'square     '(integer)         (lambda (x)   (tag (* x x))))
(put 'sine       '(integer)         (lambda (x)   (make-real(sin x))))
(put 'cosine     '(integer)         (lambda (x)   (make-real(cos x))))
(put 'arctan     '(integer integer) (lambda (x y) (make-real(atan x y))))


; in the rational package
(define (ratio x) (/ (numer x) (denom x)))
(put 'sq-root    '(rational)           (lambda (x)   (make-real (sqrt (ratio x)))))
(put 'square     '(rational)           (lambda (x)   (tag (mul-rat x x))))
(put 'sine       '(rational)           (lambda (x)   (make-real (sin (ratio x)))))
(put 'cosine     '(rational)           (lambda (x)   (make-real (cos (ratio x)))))
(put 'arctan     '(rational rational)  (lambda (x y) (make-real (atan (ratio x) (ratio y)))))


; in the real package
(put 'sq-root    '(real)       (lambda (x)   (tag (sqrt x))))
(put 'square     '(real)       (lambda (x)   (tag (* x x))))
(put 'sine       '(real)       (lambda (x)   (tag (sin x))))
(put 'cosine     '(real)       (lambda (x)   (tag (cos x))))
(put 'arctan     '(real real)  (lambda (x y) (tag (atan x y))))

; Finally `apply-generic` needs to be able to reduce the result of these new operators 
; by calling drop on them.

(define (apply-generic op . args)
  (define (reduce-type x)
    (cond ((eq? op 'add)     (drop x))
          ((eq? op 'sub)     (drop x))
          ((eq? op 'mul)     (drop x))
          ((eq? op 'div)     (drop x))
          ((eq? op 'square)  (drop x))
          ((eq? op 'sq-root) (drop x))
          ((eq? op 'sine)    (drop x))
          ((eq? op 'cosine)  (drop x))
          ((eq? op 'arctan)  (drop x))
          (else x)))
   
  ; find the highest type level of a list of arguments
  (define (highest-type-level args)
    (if (null? args) 
        0
        (let ((level (type-level (car args)))
              (highest (highest-type-level (cdr args))))
          (if (> level highest)
              level
              highest))))
   
  ; raise arg to the same level as target-type-level
  (define (raise-to arg target-type-level)
    (define (raise-iter current-arg)   
      (let ((arg-level (type-level current-arg)))
        (cond ((= arg-level target-type-level) current-arg)
              ((< arg-level target-type-level) (raise-iter (apply-generic 'raise current-arg)))
              (else (error "Cannot raise argument to a lower type target" arg target-type-level)))))
    (raise-iter arg))
   
  ; raise all args to a common type (the highest in the tower of types)
  ; and apply the operator to them 
  (define (apply-with-raised-types args)
    (let ((target-type-level (highest-type-level args)))
      (apply apply-generic 
             op 
             (map (lambda (arg)
                    (raise-to arg target-type-level))
                  args))))
   
  (let* ((type-tags (map type-tag args))
         (proc (get op type-tags)))
    (if proc 
        (reduce-type (apply proc (map contents args)))
        (apply-with-raised-types args))))