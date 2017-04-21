(* zED80 - EL DENDO's Z80 emulator V0.2 DEV
   (c)2010,2017 by ir. M. Dendooven
   This program is a Z80 emulator under construction
   the decoding algorithm is based on http://www.z80.info/decoding.htm *)


// known bugs:
// IN r,(C)              SZ503P0-	Also true for IN F,(C)  nyi
// lots of instructions not implemented

unit zED80;

interface

type  	readCallBack = function(address: word): byte;
	writeCallBack = procedure(address: word; value: byte);
	
procedure runZED80(PC,SP: word; peek,input: readCallBack; poke,output: writeCallBack);

implementation

procedure runZED80(PC,SP: word; peek,input: readCallBack; poke,output: writeCallBack);

const
	debug = true;
	step = false;
	
	r : array [0..7] of string = ('B','C','D','E','H','L','(HL)','A');
	rp : array [0..3] of string = ('BC','DE','HL','SP');
	rp2 : array [0..3] of string = ('BC','DE','HL','AF');
	cc : array [0..7] of string = ('NZ','Z','NC','C','PO','PE','P','M');
	alu : array [0..7] of string = ('ADD A','ADC A','SUB','SBC A','AND','XOR','OR','CP');
//	rot : array [0..7] of string = ('RLC','RRC','RL','RR','SLA','SRA','SLL','SRL');	

type 	oneBit = 0..1;
	twoBits = 0..3;
	threeBits = 0..7; 
	flagNames = (FC,FN,FPV,F3,FH,F5,FZ,FS); // 7..0
var
//	PC, SP : word; //program counter, Stack pointer

	IR, A: byte; //instruction register, Accumulator
	F:	packed record	 //Flagregister
		    case boolean of
			    true: (b: bitPacked array[flagNames] of boolean);
			    false:(reg: byte);
		end;

	
	reg: 	packed record // other registers
		    case boolean of
			    true: (b: array [0..5] of byte); // B C D E H L
			    false: (w: array [0..2] of word);// BC DE HL       
		end;
			
	x,p : twoBits; 		//decoder-
	y,z : threeBits;	//help-
	q : oneBit;		//variables
		
	instr: string = 'none'; //container for instruction in assembly language
	
	n: cardinal; //count instructions 

function peek2(address: word): word;
begin
	peek2 := peek(address) + peek(address+1) * 256;
end;

procedure poke2(address, val: word);
begin
	poke(address,lo(val));
	poke(address+1,hi(val))
end;

procedure wr8(r: threeBits; val: byte); // write to 8bits reg
begin
	case r of
		6:	poke(reg.w[2],val); // (HL) <- val
		7:	A := val;
		else reg.b[r] := val
	end
end;

function rd8(r: threeBits): byte; // read from 8bits reg
begin
	case r of
		6:	rd8 := peek(reg.w[2]); // val <- (HL)
		7:	rd8 := A;
		else rd8 := reg.b[r]
	end
end;

procedure wr16_rp (rp: twoBits; val: word);
begin
	if rp = 3 then SP := val else reg.w[rp] := val	
end;

function rd16_rp (rp: twoBits): word;
begin
	if rp = 3 then rd16_rp := SP else rd16_rp := reg.w[rp]
end;

procedure wr16_rp2 (rp2: twoBits; val: word);
begin
	if rp2 = 3 	then begin A := lo(val); F.reg := hi(val) end 
				else reg.w[rp2] := val	
end; 

function rd16_rp2 (rp2: twoBits): word;
begin
	if rp2 = 3 then rd16_rp2 := A*256+F.reg else rd16_rp2 := reg.w[rp2]
end;

function imm8: byte;
begin
	imm8 := peek(PC);
	inc(PC)
end;

function imm16: word;
begin
	imm16 := peek2(PC);
	inc(PC,2)
end;

function disp: shortint; // signed 8 byte type
begin
	disp := peek(PC);
	inc(PC)
end;


procedure nyi(s: string);
begin
	writeln;
	writeln('***************************************');
	writeln(' the instruction: ',s);
	writeln(' is not yet implemented');
	writeln('***************************************');
	HALT
end;

procedure monitor;
var i: flagNames;
begin
	if debug then
	begin
		writeln; writeln('previous instruction: ',instr);writeln;
		inc(n);
		writeln('nr. of instructions executed: ',n);
		writeln('PC=',hexStr(PC-1,4),' IR=',hexStr(IR,2),' SP=',hexStr(SP,4));
		writeln('x=',x,' y=',y,' z=',z,' p=',p,' q=',q);
		writeln('A=',hexstr(A,2),' BC=',hexstr(reg.w[0],4),' DE=',hexstr(reg.w[1],4),' HL=',hexstr(reg.w[2],4));
		writeln('SZ5H3PNC');
		for i:=FS to FC do write(integer(F.b[i])); writeln;
		writeln(binstr(F.reg,8))
	end
end;

procedure Flags(VS,VZ,V5,VH,V3,VPV,VN,VC: boolean);
begin
	F.b[FS]:=VS;F.b[FZ]:=VZ;F.b[F5]:=V5;F.b[FH]:=VH;F.b[F3]:=V3;F.b[FPV]:=VPV;F.b[FN]:=VN;F.b[FC]:=VC
end;

procedure alu8(operation: threeBits; var Q: byte ;n: byte); // temporary version... flags should be added...
var H: byte;
    HH: word;
    
	function PE(B: byte): boolean; //Parity Equal
	var i,Q: byte;
	begin
	    Q := 0;
	    for i := 1 to 8 do begin Q := Q xor (B and 1); B := B >> 1 end;
	    PE := not boolean (Q);
	end;
	
	function V (SI: SmallInt): boolean; //2 complements oVerflow
	begin					//smallInt = signed 16bit
	    V := (SI > 127) or (SI < -128)
	end;
    
begin 
	case operation of
{ADD}	0: 	begin 	HH := Q+n;H := (Q and $0F)+(n and $0F); Q := HH; 
			Flags(Q>127,Q=0,boolean(Q and $20),H>15,boolean(Q and $08),
			V(HH),false,HH>255) 
		end; //SZ5H3VNC
{ADC}	1:	begin 	HH := Q+n+byte(F.b[FC]);H := (Q and $0F)+(n and $0F)+byte(F.b[FC]); Q := HH; 
			Flags(Q>127,Q=0,boolean(Q and $20),H>15,boolean(Q and $08),
			V(HH),false,HH>255) 
		end; //SZ5H3VNC
{SUB}	2:	begin 	HH := Q-n;H := (Q and $0F)-(n and $0F); Q := HH; 
			Flags(Q>127,Q=0,boolean(Q and $20),H>15,boolean(Q and $08),
			V(HH),true,HH>255) 
		end; //SZ5H3VNC
{SBC}	3:	begin 	HH := Q-n-byte(F.b[FC]);H := (Q and $0F)-(n and $0F)-byte(F.b[FC]); Q := HH; 
			Flags(Q>127,Q=0,boolean(Q and $20),H>15,boolean(Q and $08),
			V(HH),true,HH>255) 
		end; //SZ5H3VNC
{AND}	4:	begin 	Q := Q and n;
			Flags(Q>127,Q=0,boolean(Q and $20),true,boolean(Q and $08),
			PE(Q),false,false)
		end; //SZ513P00
{XOR}	5:	begin 	Q := Q xor n;
			Flags(Q>127,Q=0,boolean(Q and $20),false,boolean(Q and $08),
			PE(Q),false,false) 
		end; //SZ503P00
{OR}	6:	begin 	Q := Q or n;
			Flags(Q>127,Q=0,boolean(Q and $20),false,boolean(Q and $08),
			PE(Q),false,false)
		end; //SZ503P00
{CP}	7:	begin 	HH := Q-n; H := (Q and $0F)-(n and $0F);
			Flags(HH>127,Q=0,boolean(n and $20),H>15,boolean(n and $08),
			V(HH),true,HH>255) 
		end; //SZ*H*VNC
	end
end;

procedure ADD16 (var QQ: word ; nn: word);
var R,P: byte;
    c : boolean;
begin
	c := F.b[FC]; //save carry
	P := lo(QQ); alu8(1,P,lo(nn)); //add P, lo(nn)
	R := hi(QQ); alu8(2,R,hi(nn)); //adc R, hi(nn)
	QQ := R*256+P;
	F.b[FC] := c //restore carry	
end;

procedure push16(nn: word);
begin
	dec(SP,2);
	poke2(SP,nn);
end;

function pop16: word;
begin
	pop16 := peek2(SP);
	inc(SP,2)
end;

function testcc(n: threeBits): boolean;
begin
	case n of
		0:testcc := not F.b[FZ];
		1:testcc := F.b[FZ];
		2:testcc := not F.b[FC];
		3:testcc := F.b[FC];
		4:testcc := not F.b[FPV];
		5:testcc := F.b[FPV];
		6:testcc := not F.b[FS];
		7:testcc := F.b[FS];
	end
end;

procedure oneInstr;
var QQ: word;
    c: boolean;
begin
	x := (IR and $C0) shr 6;
	y := (IR and $38) shr 3;
	z :=  IR and $07;
	p := (y and $06) shr 1;
	q :=  y and $01;
	monitor;
	case x of
	  0: case z of
		0: case y of
			0: instr := 'NOP';
			1: nyi('EX AF,AF''');
			2: nyi('DJNZ disp');
			3: begin instr := 'JR disp'; PC := PC + disp + 1 end;
			4..7: 	begin 
				    instr := 'JR '+cc[y-4]+', disp'; 
				    if testcc(y-4)  then PC := PC + disp + 1
						    else inc(PC)
				end
		   end;
		1: case q of
			0: begin instr := 'LD '+rp[p]+', imm16'; wr16_rp(p,imm16) end;
			1: begin instr := 'ADD HL '+rp[p]; QQ :=rd16_rp(2); ADD16(QQ,rd16_rp(p)); wr16_rp(2,QQ) end
		   end;
		2: case q of
			0:case p of
				0: nyi('LD (BC),A');
				1: nyi('LD (DE),A');
				2: begin instr := 'LD ('+'imm16'+'),HL'; poke2(imm16,rd16_rp(2)) end;
				3: begin instr := 'LD ('+'imm16'+'),A'; poke(imm16,A) end
			  end;
			1:case p of
				0: nyi('LD A,(BC)');
				1: nyi('LD A,(DE)');
				2: begin instr := 'LD HL,(imm16)'; wr16_rp(2,peek2(imm16)) end;
				3: begin instr := 'LD A,(imm16)'; A := peek(imm16) end;
			  end
		   end;
		3: case q of
			0: 	begin 
					instr := 'INC '+rp[p]; 
					QQ := rd16_rp(p);
					ADD16(QQ,1);
					wr16_rp(p,QQ)
				end;
			1: nyi('DEC '+rp[p]);
		   end;
		4: nyi('INC '+r[y]);
		5: nyi('DEC '+r[y]);
		6: begin instr := 'LD '+r[y]+', imm8'; wr8(y,imm8) end;
		7: case y of
			0: nyi('RLCA');
			1: nyi('RRCA');
			2: nyi('RLA');
			3: begin instr := 'RRA'; c := boolean(A xor 1); A := A >> 1; A := A and (F.reg << 7); F.b[FC] := c end;
			4: nyi('DAA');
			5: nyi('CPL');
			6: nyi('SCF');
			7: nyi('CCF')
		   end
		end;
	  1:if (z=6) and (y=6) 	then begin instr := 'HALT'; HALT end 
				else begin instr := 'LD '+r[y]+','+r[z]; wr8(y,rd8(z)) end;
	  2:begin instr := alu[y]+' '+r[z]; alu8(y,A,rd8(z))  end;
	  3:case z of
		0:nyi('RET '+cc[y]);
		1:case q of
			0:begin instr := 'POP '+rp2[p]; wr16_rp2(p, pop16) end;
			1:case p of
				0:begin instr := 'RET'; PC := pop16 end;
				1:nyi('EXX **');
				2:nyi('JP HL');
				3:nyi('LD SP,HL')
			  end
		  end;
		2:begin instr := 'JP '+cc[y]+','+'imm16'; if testcc(y) then PC := imm16 else inc(PC,2) end;
		3:case y of
			0:begin instr := 'JP imm16'; PC := imm16 end;
			1:nyi('** CB prefix **');
			2:begin instr := 'OUT (imm8),A'; output(imm8,A) end;
			3:begin instr := 'IN A,(imm8)'; A := input(imm8) end; //flags ???
			4:nyi('EX (SP),HL');
			5:begin instr := 'EX DE,HL'; QQ := rd16_rp(1); wr16_rp(1,rd16_rp(2)); wr16_rp(2,QQ) end;
			6:nyi('DI');
			7:instr := 'EI' // does nothing here 
		  end;
		4:nyi('CALL '+cc[y]+',imm16');
		5:case q of
			0:begin instr := 'PUSH '+rp2[p]; push16(rd16_rp2(p)) end;
			1:case p of
				0:begin instr := 'CALL imm16'; push16(PC+2); PC := imm16  end;
				1:nyi(' ** DD prefix ** ');
				2:nyi(' ** ED prefix ** ');
				3:nyi(' ** FD prefix ** ')
			  end
		  end;
		6:begin instr := alu[y]+' imm8'; alu8(y,A,imm8) end;
		7:nyi('RST '+hexstr(y*8,2))
		end
	end;
end;




//procedure runZED80(PC,SP: word; peek,input: readCallBack; poke,output: writeCallBack);
	
begin //runZED80
	writeln('--------------------------------------------------');
	writeln(' zED80 - EL DENDO''s Z80 emulator V0.2 DEV');
	writeln(' (c)2010,2017 by ir. M. Dendooven');
	writeln(' This program is a Z80 emulator under construction');
	writeln('--------------------------------------------------');	
	
	while true do 
	begin
		IR := peek(PC);
		inc(PC);
		oneInstr;
		if step then begin writeln; writeln('press return'); readln; end
	end
end;
end.

