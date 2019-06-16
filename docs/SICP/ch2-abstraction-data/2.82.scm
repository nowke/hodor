; Show how to generalize `apply-generic` to handle coercion in the general case of
; multiple arguments. One strategy is to attempt to coerce all the arguments to 
; the type of the first argument, then to the type of the second argument, and so 
; on. Give an example of a situation where this strategy (and likewise the 
; two-argument version given above) is not sufficiently general. (Hint: Consider 
; the case where there are some suitable mixed-type operations present in the table 
; that will not be tried.)

; Source: [https://wizardbook.wordpress.com/2010/12/08/exercise-2-82/](https://wizardbook.wordpress.com/2010/12/08/exercise-2-82/)

(define (apply-generic op . args)
   
  (define (all-coercable? coerce-procs)
    (not (member #f coerce-procs)))
   
  (define (coerce-args coercion-procs args)
    (map (lambda (coerce-proc arg) 
           (coerce-proc arg))
         coercion-procs
         args))
   
  ; attempt to coerce all args into a common type among the args
  (define (apply-with-coercion arg-types)
     
    ; attempt to coerce all args using each tag-type in turn
    ; it's a scoped procedure to keep the original arguments (arg-types) for error reporting
    (define (coerce-types tags)
      (if (null? tags)   ; all targets exhausted 
          (error "No method for these types - APPLY-GENERIC"
                 (list op arg-types))
          (let* ((target-type (car tags))
                 (arg-coercions (map        ; get all the coercion procedures from the target 
                                 (lambda (coerce-from) 
                                   (if (eq? coerce-from target-type)
                                       identity
                                       (get-coercion coerce-from target-type)))
                                 arg-types))) 
            (if (all-coercable? arg-coercions) 
                ; the target type is valid if all the args can be coerced 
                (apply apply-generic  
                       op 
                       (coerce-args arg-coercions args))
                ; target-type is not valid, so try the next one in the list
                (coerce-types (cdr tags))))))        ; try the next target type
     
    (coerce-types arg-types))
   
  (let* ((type-tags (map type-tag args))
         (proc (get op type-tags)))
    (if proc 
        (apply proc (map contents args))
        (apply-with-coercion type-tags))))