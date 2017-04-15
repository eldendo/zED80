(* Convert opcode to mnemonic for Z80
   Prefixes are not implemented (yet)	
   (c)2010 by ir. M. Dendooven
   This program is a first step towards a Z80 emulator 
   the decoding algorithm is based on http://www.z80.info/decoding.htm *)
   
program convertZ80;

const	
	r : array [0..7] of string = ('B','C','D','E','H','L','(HL)','A');
	rp : array [0..3] of string = ('BC','DE','HL','SP');
	rp2 : array [0..3] of string = ('BC','DE','HL','AF');
	cc : array [0..7] of string = ('NZ','Z','NC','C','PO','PE','P','M');
	alu : array [0..7] of string = ('ADD A','ADC A','SUB','SBC A','AND','XOR','OR','CP');
//	rot : array [0..7] of string = ('RLC','RRC','RL','RR','SLA','SRA','SLL','SRL');	

var
	OPC : BYTE;
	x,p : 0..3;
	y,z : 0..7;
	q : 0..1;

begin
	writeln('------------------------------------------');
	writeln('Convert opcode to mnemonic for Z80        ');
	writeln('(c)2010 by ir. M.Dendooven                ');
	writeln('------------------------------------------');



	for OPC := 0 to 255 do
	begin
		write(hexStr(OPC,2),': ');
		x := (OPC and $C0) shr 6;
		y := (OPC and $38) shr 3;
		z :=  OPC and $07;
		p := (y and $06) shr 1;
		q :=  y and $01;
		case x of
		  0: case z of
			0: case y of
				0: write('NOP');
				1: write('EX AF,AF''');
				2: write('DJNZ ','disp');
				3: write('JR ','disp');
				4..7: write('JR ',cc[y-4],',','disp')
			   end;
			1: case q of
				0: write('LD ',rp[p],',','imm16');
				1: write('ADD HL ', rp[p])
			   end;
			2: case q of
				0:case p of
					0: write('LD (BC),A');
					1: write('LD (DE),A');
					2: write('LD (','imm16','),HL');
					3: write('LD (','imm16','),A')
				  end;
				1:case p of
					0: write('LD A,(BC)');
					1: write('LD A,(DE)');
					2: write('LD HL,(','imm16',')');
					3: write('LD A,(','imm16',')');
				  end
			   end;
			3: case q of
				0: write ('INC ',rp[p]);
				1: write ('DEC ',rp[p]);
			   end;
			4: write ('INC ',r[y]);
			5: write ('DEC ',r[y]);
			6: write ('LD ',r[y],',','imm8');
			7: case y of
				0: write('RLCA');
				1: write('RRCA');
				2: write('RLA');
				3: write('RRA');
				4: write('DAA');
				5: write('CPL');
				6: write('SCF');
				7: write('CCF')
			   end
		    end;
		  1:if (z=6) and (y=6) then write('HALT') else write('LD ',r[y],',',r[z]);
		  2:write(alu[y],' ',r[z]);
		  3:case z of
			0:write('RET ',cc[y]);
			1:case q of
				0:write('POP ',rp2[p]);
				1:case p of
					0:write('RET');
					1:write('EXX **');
					2:write('JP HL');
					3:write('LD SP,HL')
				  end
			  end;
			2:write('JP ',cc[y],',','imm16');
			3:case y of
				0:write('JP ','imm16');
				1:write('** CB prefix **');
				2:write('OUT (','imm8','),A');
				3:write('IN A,(','imm8',')');
				4:write('EX (SP),HL');
				5:write('EX DE,HL');
				6:write('DI');
				7:write('EI')
      			  end;
			4:write('CALL ',cc[y],',','imm16');
			5:case q of
				0:write('PUSH ',rp2[p]);
				1:case p of
					0:write('CALL ','imm16');
					1:write (' ** DD prefix ** ');
					2:write (' ** ED prefix ** ');
					3:write (' ** FD prefix ** ')
				  end
			  end;
			6:write(alu[y],' ','imm8');
			7:write('RST ',hexstr(y*8,2))
		    end
		end;
	writeln
	end
end.

