; Using the raise operation of Exercise 2.83, modify the apply-generic procedure so 
; that it coerces its arguments to have the same type by the method of successive 
; raising, as discussed in this section. You will need to devise a way to test which 
; of two types is higher in the tower. Do this in a manner that is “compatible” with 
; the rest of the system and will not lead to problems in adding new levels to the 
; tower.

; Source: [https://wizardbook.wordpress.com/2010/12/08/exercise-2-84/](https://wizardbook.wordpress.com/2010/12/08/exercise-2-84/)

(define (apply-generic op . args)
   
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
        (apply proc (map contents args))
        (apply-with-raised-types args))))

; This uses a new top-level generic procedure to find the level of the number type 
; and a new procedure for each of the number types.
(define (type-level z) (apply-generic 'type-level z))
(put 'type-level '(integer) (lambda (x) 1))
(put 'type-level '(rational) (lambda (x) 2))
(put 'type-level '(real) (lambda (x) 3))
(put 'type-level '(complex) (lambda (x) 4))