(* zEDzero - EL DENDO's very simple computer emulation using a Z80
(c) copyright 2010-2017 by ir. Marc Dendooven
this is a VERY SIMPLE Z80 based computer emulation demo for my zED80
emulator *)

program zEDzero;

uses zED80, crt; // include the zED80 emulator (and the crt module for terminal io)

    var	mem: array[0..$FFFF] of byte; 		// define memory 
    
    function peek(address: word): byte; 	// define read access functionality for memory
    begin					// if memory mapped input is used include this here
	    peek := mem[address]
    end;

    procedure poke(address: word; value: byte); // define write access functionality for memory
    begin					// if memory mapped output is used include this here
	    mem[address] := value
    end;

    procedure output(port: word; value: byte);	// define output acces functionality (using the OUT instruction)
    begin					// in this demo every ouput operation writes a chraracter 
						// to the terminal
	    write(chr(value))
    end;

    function input (port: word):byte;		// define input acces functionality (using the IN instruction)
    begin					 
	    case lo(port) of
		    0: if keypressed then input := ord(readkey); 	// in this demo port 0 reads an input key	
		    1: if keypressed then input := $FF else input := 0; // port one reads if a key has been pressed
		    else input := $FF					// all other ports read all inputs 'high'
	    end
    end;
    
    procedure load_prg (filename: string; address: word); // add your own stuff
    var f : file of byte;				  // this routine reads a file into memory
        b : byte;
        i : cardinal = 0;
        a : cardinal; 
    begin
        a := address;
        {$i-}
        assign (f,filename);
        reset(f);
        {$i+}
        if ioresult <> 0
        then
                writeLn('No file named '+filename)
        else
           begin
                while not eof(f) do
                    begin
                        read(f,b);
                        mem[address] := b;
                        inc(address);
                        inc(i)
                    end;
                close(f);
//                writeln('program '+filename+' loaded. ',i,' bytes ',ceil(i/256),' pages from $',hexstr(a,4),' to $',hexstr(address-1,4))
		    writeln('program '+filename+' loaded. ',i,' bytes from $',hexstr(a,4),' to $',hexstr(address-1,4))
           end;
    end;
    
begin
    writeln('*************************************************');
    writeln('* zEDzero minimal computer emulator             *');
    writeln('* this is a demo using the zED80 emulator       *');
    writeln('* (c) copyright 2010-2017 by ir. Marc Dendooven *');
    writeln('*************************************************');
    writeln;
    
    load_prg('rom',0);					// in this demo a file called 'rom' is loaded at address 0
    runZED80(0, $E000, @peek, @input, @poke, @output)	// start the emulation with PC = 0, SP = $E000
end.							// and add your memory and io functions
