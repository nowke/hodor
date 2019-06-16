; Eva Lu Ator, another user, has also noticed the different 
; intervals computed by different but algebraically equivalent 
; expressions. She says that a formula to compute with intervals 
; using Alyssa’s system will produce tighter error bounds if it 
; can be written in such a form that no variable that represents 
; an uncertain number is repeated. Thus, she says, par2 is a 
; “better” program for parallel resistances than par1. Is she right? 
; Why?

; Eva's hypothesis seems to be correct. From our previous exercise,
; we found that A/A is not exactly "1", hence the more we avoid 
; repitition, more accurate our operations can be. Although, I am 
; not entirely sure of this answer! There can be cases where this is
; not true