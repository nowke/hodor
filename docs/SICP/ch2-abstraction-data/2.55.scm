; Eva Lu Ator types to the interpreter the ex-pression

(car ''abracadabra)

; To her surprise, the interpreter prints back quote. Explain.

; Interpreter treats ''abracadabra as
;          '(quote abracadabra) 
; Hence, (car ...) gives "quote"