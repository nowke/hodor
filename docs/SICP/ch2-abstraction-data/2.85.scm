; This section mentioned a method for “simplifying” a data object by lowering it in 
; the tower of types as far as possible. Design a procedure drop that accomplishes
; this for the tower described in Exercise 2.83. The key is to decide, in some 
; general way, whether an object can be lowered. For example, the complex number 
; 1.5 + 0i can be lowered as far as real, the complex number 1 + 0i can be lowered 
; as far as integer, and the complex number 2 + 3i cannot be lowered at all. Here is
; a plan for determining whether an object can be lowered: Begin by defining a 
; generic operation project that “pushes” an object down in the tower. For example, 
; projecting a complex number would involve throwing away the imaginary part. Then a 
; number can be dropped if, when we project it and raise the result back to the type 
; we started with, we end up with something equal to what we started with. Show how 
; to implement this idea in detail, by writing a drop procedure that drops an object 
; as far as possible. You will need to design the various projection operations and 
; install project as a generic operation in the system. You will also need to make 
; use of a generic equality predicate, such as described in Exercise 2.79. Finally, 
; use drop to rewrite `apply-generic` from Exercise 2.84 so that it “simplifies” its 
; answers.

; Source: [https://wizardbook.wordpress.com/2010/12/08/exercise-2-85/](https://wizardbook.wordpress.com/2010/12/08/exercise-2-85/)

; There are 3 changes to make. The first is for apply-generic to use the new drop 
; procedure. Note that it doesn’t make sense to drop the result of all generic 
; operations, for example predicates, and that drop is only called after any type 
; coercion using raise has completed.
(define (apply-generic op . args)
   
  ; only certain operations will result in an answer that can be
  ; projected e.g. it makes no sense to project the answer to zero?
  (define (reduce-type x)
    (cond ((eq? op 'add) (drop x))
          ((eq? op 'sub) (drop x))
          ((eq? op 'mul) (drop x))
          ((eq? op 'div) (drop x))
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

; Second implement drop and the top-level generic project.

(define (project z)  (apply-generic 'project z))
(define (drop z)
  (if (= (type-level z) 1) 
      z
      (let ((projected (project z)))
        (if (equ? z (raise projected))
            (drop projected)
            z))))

; Third, add type specific project procedures for each type that can project.

; in the rational package
(define (project r) 
    (make-integer (truncate (/ (numer r) (denom r)))))
(put 'project    '(rational) (lambda (x) (project x)))

; in the real package - this is a bit messy because real numbers can be either 
; integers, rational or irrational.
(define (project r) 
    (let ((exact (inexact->exact r)))
      (cond ((integer? exact)  (make-rational exact 1))
            ((rational? exact) (make-rational (numerator exact) (denominator exact)))
            (else (make-rational (truncate exact) 1)))))
(put 'project    '(real) (lambda (x) (project x)))

; in the complex package
(define (project z1)
  (make-real (real-part z1)))
(put 'project    '(complex) (lambda (x) (project x)))
