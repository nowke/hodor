; THe internal procedures in the scheme-number package are essentially nothing more 
; than calls to the primitive procedures +, -, etc. It was not possible to use the 
; primitives of the language directly because our `type-tag` system requires that
; each data object have a type attached to it. In fact, however, all Lisp
; implementations do have a type system, which they use internally. Primitive 
; predicates such as `symbol?` and `number?` determine whether data objects have 
; particular types. Modify the definitions of `type-tag`, `contents`, and `attach-tag` 
; from Section 2.4.2 so that our generic system takes advantage of Scheme’s 
; internal type system. That is to say, the system should work as before except that
; ordinary numbers should be represented simply as Scheme numbers rather than as 
; pairs whose car is the symbol `scheme-number`

(define (attach-tag type-tag contents) 
    (if (eq? type-tag 'scheme-number) 
        contents 
        (cons type-tag contents)))

(define (type-tag datum) 
    (cond ((number? datum) datum) 
          ((pair? datum) (car datum)) 
          (else (error "Wrong datum TYPE-TAG" datum)))) 

(define (contents datum) 
    (cond ((number? datum) datum) 
          ((pair? datum) (cdr datum)) 
          (else (error "Wrong datum CONTENGS" datum)))) 

; Testing
(attach-tag 'cos 0.5) ;Value 2: (cos . .5)
(attach-tag 'scheme-number 10) ;Value: 10

(type-tag (attach-tag 'cos 0.5)) ;Value: cos
(type-tag (attach-tag 'scheme-number 10)) ;Value: 10

(contents (attach-tag 'cos 0.5)) ;Value: .5
(contents (attach-tag 'scheme-number 10)) ;Value: 10