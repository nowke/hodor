; Louis Reasoner is having a terrible time doing Exercise 2.42. His queens 
; procedure seems to work, but it runs extremely slowly. (Louis never does 
; manage to wait long enough for it to solve even the 6 × 6 case.) When Louis 
; asks Eva Lu Ator for help, she points out that he has interchanged the order 
; of the nested mappings in the flatmap, writing it as
; 
; (flatmap
;     (lambda (new-row)
;     (map (lambda (rest-of-queens)
;     (adjoin-position new-row k rest-of-queens))
;             (queen-cols (- k 1))))
;      (enumerate-interval 1 board-size))
;
; Explain why this interchange makes the program run slowly. Estimate how long 
; it will take Louis’s program to solve the eight-queens puzzle, assuming that 
; the program in Exercise 2.42 solves the puzzle in time T.

; Answer
; ==============================================
; In Louis's program, position of `queen-cols` and `enumerate-interval` is 
; exchanged. Hence for every item in `enumerate-interval`, `queen-cols` gets
; called. Hence during every recursion, it is calculated `board-size` times
; There is unnecessary duplication of (size^size) times

; If previous program takes time T, we have (size^size) x T for Louis's program
