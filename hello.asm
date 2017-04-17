LF	equ $0A
	org 0 
begin:	CALL print
	DB 'hello world',LF
	DB '-----------',LF
	DB 'this is a simple test',LF
	DB 'for the zEDone virtual computer',LF
	DB 'using the zED80 emulator',LF,$00
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
