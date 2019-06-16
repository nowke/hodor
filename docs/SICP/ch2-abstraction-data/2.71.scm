; Suppose we have a Huffman tree for an alphabet of n symbols, and that the 
; relative frequencies of the symbols are $1, 2, 4, . . . , 2^{n−1}$. Sketch the 
; tree for n = 5; for n = 10. In such a tree (for general n) how many bits 
; are required to encode the most frequent symbol? The least frequent symbol?


; For n=5
;                  (a b c d e) 31
;                 /           \
;             (a b c d) 15    [e 16]
;               /      \      
;          (a b c) 7   [d 8]
;         /       \ 
;       (a b) 3     [c 4]
;       /    \
;     [a 1]   [b 2]

; Number of bits for most frequent symbol  = 1
; Number of bits for least frequent symbol = n - 1