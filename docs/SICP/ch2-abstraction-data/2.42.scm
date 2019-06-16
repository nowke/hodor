; The “eight-queens puzzle” asks how to place eight queens on a chessboard so that 
; no queen is in check from any other (i.e., no two queens are in the same row, 
; column, or diagonal). One possible solution is shown in Figure 2.8. One way to 
; solve the puzzle is to work across the board, placing a queen in each column. 
; Once we have placed k−1 queens, we must place the kth queen in a position where 
; it does not check any of the queens already on the board. We can formulate this 
; approach recursively: Assume that we have already generated the sequence of all 
; possible ways to place k − 1 queens in the first k − 1 columns of the board. For 
; each of these ways, generate an extended set of positions by placing a queen in 
; each row of the kth column. Now filter these, keeping only the positions for 
; which the queen in the kth column is safe with respect to the other queens. This 
; produces the sequence of all ways to place k queens in the first k columns. 
; By continuing this process, we will produce not only one solution, but all 
; solutions to the puzzle.
; We implement this solution as a procedure queens, which returns a sequence of all 
; solutions to the problem of placing n queens on an n × n chessboard. queens has 
; an internal procedure queen-cols that returns the sequence of all ways to place 
; queens in the first k columns of the board.
(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
          (lambda (positions) (safe? k positions))
          (flatmap
            (lambda (rest-of-queens)
              (map (lambda (new-row)
                     (adjoin-position 
                        new-row k rest-of-queens))
                   (enumerate-interval 1 board-size)))
            (queen-cols (- k 1))))))
  (queen-cols board-size))

; In this procedure rest-of-queens is a way to place k − 1 queens in the first 
; k − 1 columns, and new-row is a proposed row in which to place the queen for 
; the kth column. Complete the program by implementing the representation for 
; sets of board positions, including the procedure adjoin-position, which adjoins 
; a new row-column position to a set of positions, and empty-board, which represents 
; an empty set of positions. You must also write the procedure safe?, which 
; determines for a set of positions, whether the queen in the kth column is safe 
; with respect to the others. (Note that we need only check whether the new queen 
; is safe — the other queens are already guaranteed safe with respect to each other.)

; Define `enumerate-interval`, and `flatmap` procedures
(define nil '())
(define (enumerate-interval low high)
    (if (> low high)
        nil
        (cons low (enumerate-interval (+ low 1) high))))
(define (accumulate op initial sequence)
    (if (null? sequence) 
        initial
        (op (car sequence) (accumulate op initial (cdr sequence)))))
(define (flatmap proc seq)
    (accumulate append nil (map proc seq)))

; (i) Define `empty board`
(define empty-board nil)

; (ii) Define `safe?` procedure
; 
; Let's solve the problem part by part. First of all, the data structure
; of `positions`, i.e. one of the argument to `safe?` is as follows
; 
; (((1 1) (3 2) (5 3) (1 4))
; This means, column 1 => (pos 1)
;             column 2 => (pos 3)
;             column 3 => (pos 5)
;         kth-column 4 => (pos 1)
; i.e. Column 1-3 => queens are already safe
;        and we want to test at k=4, 4th column and 
;        whether position "1" is safe or not
;
; We can check against each of Col 1-3, for any dangers
; Here, danger for `kth-column` from `nth-col` means 3 conditions
;     1) Position of nth-col = Position of kth-col => Same row
;     2) Diagonal of nth-col in kth-col = kth-col
;     3) Skew-diagonal of nth-col in kth-col = kth-col 

(define (row-diag row-val row k)
    (+ row-val (- k row)))
(define (skew-diag row-val row k)
    (- row-val (- k row)))

(define (pair-safe? p1 p2)
    (define row     (cadr p1))
    (define row-val (car p1))
    (define k       (cadr p2))
    (define k-val   (car p2))
    (not 
      (or (= row-val k-val)
          (= (row-diag  row-val row k) k-val)
          (= (skew-diag row-val row k) k-val))))

(define (safe? k positions)
    ; ((1 1) (3 2) (5 3) (1 4)) => (1 4) - test-value
    (define test-value (last positions))
    (define (iter x positions)
        (cond ((= x k) #t)
              ((pair-safe? (car positions) test-value)
               (iter (+ x 1) (cdr positions)))
              (else #f)
        )
    )
    (iter 1 positions))

; In the setting, ((1 1) (3 2) (5 3))
; we can test which row suites for 4th column (k = 4)
; If we draw the board, we can observe that 2nd row is the only 
; suitable position
; Let's test this with `safe?` procedure

(safe? 4 (list (list 1 1) (list 3 2) (list 5 3) (list 1 4))) ;Value: #f
(safe? 4 (list (list 1 1) (list 3 2) (list 5 3) (list 2 4))) ;Value: #t
(safe? 4 (list (list 1 1) (list 3 2) (list 5 3) (list 3 4))) ;Value: #f
(safe? 4 (list (list 1 1) (list 3 2) (list 5 3) (list 4 4))) ;Value: #f
(safe? 4 (list (list 1 1) (list 3 2) (list 5 3) (list 5 4))) ;Value: #f

; (iii) Define `adjoin-position` procedure
; Example
; ----------------------
; Input: new-row        = 1
;        k              = 4
;        rest-of-queens = ((1 1) (3 2) (5 3))       
; Output: ((1 1) (3 2) (5 3) (1 4))
(define (adjoin-position new-row k rest-of-queens)
    (append rest-of-queens (list (list new-row k))))

; Testing for queens 4, 5, 6
(queens 4)
; (((2 1) (4 2) (1 3) (3 4)) 
;  ((3 1) (1 2) (4 3) (2 4))) => 2 solutions

(queens 5)
; (((1 1) (3 2) (5 3) (2 4) (4 5)) 
;  ((1 1) (4 2) (2 3) (5 4) (3 5)) 
;  ((2 1) (4 2) (1 3) (3 4) (5 5)) 
;  ((2 1) (5 2) (3 3) (1 4) (4 5)) 
;  ((3 1) (1 2) (4 3) (2 4) (5 5)) 
;  ((3 1) (5 2) (2 3) (4 4) (1 5))
;  ((4 1) (1 2) (3 3) (5 4) (2 5))
;  ((4 1) (2 2) (5 3) (3 4) (1 5)) 
;  ((5 1) (2 2) (4 3) (1 4) (3 5)) 
;  ((5 1) (3 2) (1 3) (4 4) (2 5))) => 10 solutions

(queens 6)
; (((2 1) (4 2) (6 3) (1 4) (3 5) (5 6)) 
;  ((3 1) (6 2) (2 3) (5 4) (1 5) (4 6)) 
;  ((4 1) (1 2) (5 3) (2 4) (6 5) (3 6)) 
;  ((5 1) (3 2) (1 3) (6 4) (4 5) (2 6))) => 4 solutions
