; Suppose we evaluate the expression (list 1 (list 2 (list 3 4))). Give the 
; result printed by the interpreter, the corresponding box-and-pointer 
; structure, and the interpretation of this as a tree.

(list 1 (list 2 (list 3 4)))

; (i) Result printed by interpreter
;    (1 (2 (3 4)))

; (ii) Box-and-pointer structure
;                    ---------      ---------        ---------
;   (1 (2 (3 4))) -> | * | *-|----> | * | *-|------> | * | *-|---> (4, null)
;                    ---------      ---------        ---------
;                      |              |                |
;                      v              v                v
;                   (1, null)       (2, null)       (3, null)

; (iii) Tree structure
;          (1 (2 (3 4)))
;               *
;          (1) *        * (2 (3 4))
;                 (2) *    * (3 4)
;                      (3)*  *(4) 