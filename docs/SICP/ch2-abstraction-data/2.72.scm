; Consider the encoding procedure that you designed in Exercise 2.68. What is the 
; order of growth in the number of steps needed to encode a symbol? Be sure to 
; include the number of steps needed to search the symbol list at each node 
; encountered. To answer this question in general is difficult. Consider the 
; special case where the relative frequencies of the n symbols are as described 
; in Exercise 2.71, and give the order of growth (as a function of n) of the number 
; of steps needed to encode the most frequent and least frequent symbols in the 
; alphabet.

; ==================================================================
; Source: http://community.schemewiki.org/?sicp-ex-2.72
; ==================================================================

; For the encode-symbol procedure in 2.68:

; Search the symbol list at each node: O(n) time
; Then take log_n branches
; Total: O(n * log_n)
; For the special case described in 2.71:

; 1. Encoding the most frequent symbol:

; Search through symbol list: O(n) time
; Take the first single branch, since it will be at the top of the list: constant
; Total: O(n)
; 2. Encoding the least frequent symbol:

; Search through symbol list at each level: O(n) time
; Take the next branch, since we are only removing one node, it would be: O(n - 1)
; Total: O(n * (n - 1)), or O(n^2)