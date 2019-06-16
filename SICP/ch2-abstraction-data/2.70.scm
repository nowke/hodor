; The following eight-symbol alphabet with associated relative frequencies was 
; designed to efficiently encode the lyrics of 1950s rock songs. (Note that the 
; “symbols” of an “alphabet” need not be individual letters.)
; ```
; A    2   GET 2   SHA 3   WAH 1 
; BOOM 1   JOB 2   NA 16   YIP 9
; ```
; Use `generate-huffman-tree` (Exercise 2.69) to generate a corresponding Huffman 
; tree, and use encode (Exercise 2.68) to encode the following message:
; Get a job
; Sha na na na na na na na na
; Get a job
; Sha na na na na na na na na
; Wah yip yip yip yip yip yip yip yip yip
; Sha boom
; 
; How many bits are required for the encoding? What is the smallest number of bits 
; that would be needed to encode this song if we used a fixed-length code for the 
; eight-symbol alphabet?

; Helpers
(define (make-leaf symbol weight) (list 'leaf symbol weight))
(define (leaf? object) (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
    (list left
          right
          (append (symbols left) (symbols right))
          (+ (weight left) (weight right))))
(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))
(define (symbols tree)
    (if (leaf? tree)
        (list (symbol-leaf tree))
        (caddr tree)))
(define (weight tree)
    (if (leaf? tree)
        (weight-leaf tree)
        (cadddr tree)))
(define (decode bits tree)
    (define (decode-1 bits current-branch)
        (if (null? bits)
            '()
            (let ((next-branch
                    (choose-branch (car bits) current-branch)))
                (if (leaf? next-branch)
                    (cons (symbol-leaf next-branch)
                          (decode-1 (cdr bits) tree))
                    (decode-1 (cdr bits) next-branch)))))
    (decode-1 bits tree))
(define (choose-branch bit branch)
    (cond ((= bit 0) (left-branch branch))
          ((= bit 1) (right-branch branch))
          (else (error "bad bit: CHOOSE-BRANCH" bit))))
(define (adjoin-set x set)
    (cond ((null? set) (list x))
          ((< (weight x) (weight (car set))) (cons x set))
          (else (cons (car set) (adjoin-set x (cdr set))))))
(define (make-leaf-set pairs)
    (if (null? pairs)
        '()
        (let ((pair (car pairs)))
            (adjoin-set (make-leaf (car pair)       ; symbol
                                   (cadr pair))     ; frequency
                        (make-leaf-set (cdr pairs))))))

; Successive merge procedure
(define (successive-merge leaf-set)
    (if (null? (cdr leaf-set))
        (car leaf-set)
        (successive-merge
            (adjoin-set
                (make-code-tree (car leaf-set) (cadr leaf-set))
                (cddr leaf-set)))))
(define (generate-huffman-tree pairs) 
    (successive-merge (make-leaf-set pairs)))
(define (encode message tree) 
    (if (null? message)
        '()
        (append (encode-symbol (car message) tree)
                (encode (cdr message) tree))))
(define (encode-symbol symbol tree)
    (cond 
        ((leaf? tree) '())
        ((element-of-set? symbol (symbols tree))
            (let ((left  (left-branch tree))
                  (right (right-branch tree)))
                (if (element-of-set? symbol (symbols left))
                    (cons 0 (encode-symbol symbol left))
                    (cons 1 (encode-symbol symbol right)))))
        (else (error "Bad symbol" symbol))))
(define (element-of-set? x set) 
    (cond ((null? set) false) 
            ((equal? x (car set)) true) 
            (else (element-of-set? x (cdr set))))) 
                
; Answer
(define alphabet '((A 2) (GET 2) (SHA 3) (WAH 1) (BOOM 1) (JOB 2) (NA 16) (YIP 9)))
(define song-tree (generate-huffman-tree alphabet))
; ((leaf na 16) ((leaf yip 9) (((leaf a 2) ((leaf boom 1) (leaf wah 1) (boom wah) 2) 
; (a boom wah) 4) ((leaf sha 3) ((leaf job 2) (leaf get 2) (job get) 4) (sha job get) 
; 7) (a boom wah sha job get) 11) (yip a boom wah sha job get) 20) (na yip a boom wah
;  sha job get) 36)

(define song 
    '(GET A JOB
      SHA NA NA NA NA NA NA NA NA
      GET A JOB
      SHA NA NA NA NA NA NA NA NA
      WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP 
      SHA BOOM))

(define encoded-song (encode song song-tree))
(length encoded-song) ;Value: 84

; Hence "84 bits" are needed

; If we used fixed length encoding, we need 36 x 3 = "108 bits"