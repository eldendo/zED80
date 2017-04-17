LF		equ $0A
		org 0 
		JP $100
		org $100
begin:	CALL print
		DB 'hello world',LF,$00
		HALT
	
print:	POP HL
next: 	LD A,(HL)
		CP $00
		JP Z,exit
		OUT (0),A
		INC HL
		JP next

exit:	INC HL
		PUSH HL
		RET
