library ieee;
use ieee.numeric_bit.all;
use ieee.math_real.log2;
use ieee.math_real.ceil;
----------------------------------------
-- registrador simples para guardar o pc
----------------------------------------
entity reg is
    generic(
        wordSize: natural := 4
    );
    port(
        clock: in bit;
        reset: in bit; 
        load: in bit; 
        d: in  bit_vector(wordSize-1 downto 0);
        q: out bit_vector(wordSize-1 downto 0)
    );
end reg;

architecture regSave of reg is
    signal dado: bit_vector(wordSize - 1 downto 0);

    begin
        process(clock, reset)
        begin
        if reset = '1' then
            dado <= (others=>'0');
        elsif (clock'event and clock='1') then
            if load='1' then
                dado <= D;
            end if;
        end if;
        end process;

        Q <= dado;
end architecture;
------------------------------------
--banco de registradores
------------------------------------
library IEEE;
use IEEE.numeric_bit.all;
use ieee.math_real.ceil;
use ieee.math_real.log2;

entity regfile is
	generic(
		regn : natural := 32;
		wordSize: natural := 64
	);
	port(
		clock: in bit;
		reset: in bit;
		regWrite: in bit;
		rr1, rr2, wr: in bit_vector( natural( ceil( log2(real(regn)))) - 1 downto 0);
		d: in bit_vector( wordSize -1 downto 0);
		q1, q2: out bit_vector( wordSize -1 downto 0)
	);
end regfile;

architecture banco of regfile is
	type regs_t is array (0 to regn-1) of bit_vector(WordSize-1 downto 0);
	signal regs : regs_t := (others => (others => '0'));
	
begin
	process (clock, reset)
	begin
		if (reset = '1') then
			regs <= (others => (others => '0'));
		end if;
		if rising_edge(clock) then
			if (regWrite = '1' and to_integer(unsigned(wr)) /= regn-1) then
				regs ( to_integer(unsigned(wr))) <= d;
			end if;
		end if;		
	end process;
	q2 <= regs( to_integer(unsigned(rr2)));
	q1 <= regs( to_integer(unsigned(rr1)));
end architecture;

entity alu1bit is
	port ( 
		a, b, less, cin : in bit;
		result, cout, set, overflow: out bit;
		ainvert, binvert: in bit;
		operation: in bit_vector(1 downto 0)
	);
end entity;

---------------------------------------------
--ula de 64 bits formada por 64 ulas de 1 bit
---------------------------------------------
architecture ula1 of alu1bit is
	signal ainv   : bit;
	signal binv   : bit;
	signal sum : bit;
	signal cout2 : bit;
begin
	
	ainv <= a when ainvert = '0' else not a;
	binv <= b when binvert = '0' else not b;
	cout2 <= ((ainv xor binv)and cin)or ( ainv and binv);
	sum <=  binv xor ainv xor cin;
	
	set <= sum;
	cout <= cout2;
	overflow <= cin xor cout2;
	result <= ainv and binv when operation = "00" else
			  ainv or  binv when operation = "01" else
			  sum     when operation = "10" else
			  less       when operation = "11";
			  
end architecture;

entity alu is
	generic( 
		size : natural := 64
	);
	port (
		A, B : in  bit_vector(size-1 downto 0);
		F    : out bit_vector(size-1 downto 0);
		S    : in  bit_vector(3 downto 0);
		Z    : out bit;
		Ov   : out bit;
		Co   : out bit
	);

end entity;

architecture ula of alu is
	component alu1bit is
		port ( 
		a, b, less, cin : in bit;
		result, cout, set, overflow: out bit;
		ainvert, binvert: in bit;
		operation: in bit_vector(1 downto 0)
	);
	end component;
	signal soma, result, over :bit_vector(size-1 downto 0);
	signal nulo :bit_vector(size-1 downto 0) := (others => '0');
	signal cOut : bit_vector (size downto 0);
	signal  binv : bit;
begin
	-- 64 ulas de 1 bit
	alus1 : for i in  0 to size-1 generate
				alu1b : alu1bit port map (
				A(i), 
				B(i), 
				B(i), 
				cOut(i), 
				result(i),
				cOut(i+1),
				soma(i),
				over(i), 
				S(3),
				S(2), 
				S(1 downto 0)
			);
			end generate;
	--entradas das ulas de 1 bit
	cOut(0) <= S(2);  --cin da ula 0		   
	--binv <= '1' when S= "1100" or S = "0110" else '0';
	
	--saidas ula 64 bits
	Co <= cOut(size);
	Ov <= over(size-1);
	Z  <= '1' when result = nulo else '0'; 
	F  <= result;
end architecture;
-------------------------------------------------------------
-- sign extend
-------------------------------------------------------------
entity signExtend is
	port (
		i: in   bit_vector(31 downto 0); --input
		o: out  bit_vector(63 downto 0)  --output
	);
end signExtend;

architecture extend of signExtend is
	signal Dnulo     : bit_vector (54 downto 0) := (others => '0');
	signal Dum     : bit_vector (54 downto 0) := (others => '1');
	
	signal CBZnulo   : bit_vector (44 downto 0) := (others => '0');
	signal CBZum  : bit_vector (44 downto 0) := (others => '1');
	
	signal Bnulo     : bit_vector (37 downto 0) :=  (others => '0');
	signal Bum     : bit_vector (37 downto 0) :=  (others => '1');
	
begin

	 
	 o <= Dnulo   & i(20 downto 12) when (i(31 downto 21) = "11111000010" or i(31 downto 21) = "11111000000") and i(20) = '0' else 
		  Dum     & i(20 downto 12) when (i(31 downto 21) = "11111000010" or i(31 downto 21) = "11111000000") and i(20) = '1'  else 
		  CBZnulo & i(23 downto 5)  when i(31 downto 24) = "10110100" and i(23) = '0' else
		  CBZum   & i(23 downto 5)  when i(31 downto 24) = "10110100" and i(23) = '1' else
		  Bnulo   & i(25 downto 0)  when i(31 downto 26) = "000101" and i(25) = '0' else
		  Bum     & i(25 downto 0)  when i(31 downto 26) = "000101" and i(25) = '1';		
end architecture;
-----------------------------------------------------
-- alucontrol
-----------------------------------------------------
entity alucontrol is
	port (
		aluop : in bit_vector (1 downto 0);
		opcode: in bit_vector (10 downto 0);
		aluCtrl: out bit_vector (3 downto 0)
	);
end entity;

architecture controle of alucontrol is

begin
 aluCtrl <= "0010" when aluop = "00" else
			"0111" when aluop(0) = '1' else
			"0010" when aluop(1) = '1' and opcode = "10001011000" else
			"0110" when aluop(1) = '1' and opcode = "11001011000" else
			"0000" when aluop(1) = '1' and opcode = "10001010000" else
			"0001" when aluop(1) = '1' and opcode = "10101010000";
end architecture;
---------------------------------------------------------------------
-- unidade de controle:
---------------------------------------------------------------------
entity controlunit is
	port (
		--to datapath
		reg2loc      : out bit;
		uncondBranch : out bit;
		branch       : out bit;
		memRead      : out bit;
		memToReg     : out bit;
		aluOp        : out bit_vector(1 downto 0);
		memWrite     : out bit;
		aluSrc       : out bit;
		regWrite     : out bit;
		--from datapath
		opcode       : in bit_vector(10 downto 0)
	);
end entity;
architecture controlador of controlunit is
	signal intermediario : bit_vector (9 downto 0);
begin	
	intermediario <= "1001100011" when opcode = "11111000010" else --ldur
					 "1000100110" when opcode = "11111000000" else --stur
					 "1010101000" when opcode(10 downto 3) = "10110100" else --cbz
					 "1100101000" when opcode(10 downto 5) = "000101" else --b
					 "0000010001" when opcode = "10001011000" or opcode = "11001011000" or opcode = "10001010000" or opcode = "10101010000" else --r
					 "0000000000";
	
	reg2loc      <= intermediario(9);
	uncondBranch <= intermediario(8);
	branch       <= intermediario(7);
	memRead      <= intermediario(6);
	memToReg     <= intermediario(5);
	aluOp        <= intermediario(4) & intermediario(3);
	memWrite     <= intermediario(2);
	aluSrc       <= intermediario(1);
	regWrite     <= intermediario(0);
	
end architecture;
---------------------------------------------------------------
--datapath: fluxo de dados
---------------------------------------------------------------
library IEEE;
use IEEE.numeric_bit.all;
use ieee.math_real.ceil;
use ieee.math_real.log2;

entity datapath is
    port(
		--Commom
        clock:     in bit;
        reset:     in bit;
		--From Control Unit
        reg2loc:   in bit;
        pcsrc:     in bit;
        memToReg:  in bit;
        aluCtrl:   in bit_vector(3 downto 0);
        aluSrc:    in bit;
        regWrite:  in bit;
		--To controUnit
        opcode:    out bit_vector(10 downto 0);--
        zero:      out bit;--
		--IM interface 
        imAddr:    out bit_vector(63 downto 0);--
        imOut:     in  bit_vector(31 downto 0);
		--DM interface
        dmAddr:    out bit_vector(63 downto 0);--
        dmIn:      out bit_vector(63 downto 0);--
        dmOut:     in  bit_vector(63 downto 0)
    );
end entity datapath;

architecture fluxo of datapath is
	-- registrador para guardar o pc
	component reg is
    generic(
        wordSize: natural := 64
    );
    port(
        clock: in bit;
        reset: in bit; 
        load: in bit; 
        d: in  bit_vector(wordSize-1 downto 0);
        q: out bit_vector(wordSize-1 downto 0)
    );
	end component reg;
	--banco de registradores
	component regfile is
	generic(
		regn : natural := 32;
		wordSize: natural := 64
	);
	port(
		clock: in bit;
		reset: in bit;
		regWrite: in bit;
		rr1, rr2, wr: in bit_vector( natural( ceil( log2(real(regn)))) - 1 downto 0);
		d: in bit_vector( wordSize -1 downto 0);
		q1, q2: out bit_vector( wordSize -1 downto 0)
	);
	end component;
	-- unidades lógico aritméticas
	component alu is
	generic( 
		size : natural := 64
	);
	port (
		A, B : in  bit_vector(size-1 downto 0);
		F    : out bit_vector(size-1 downto 0);
		S    : in  bit_vector(3 downto 0);
		Z    : out bit;
		Ov   : out bit;
		Co   : out bit
	);

	end component;
	--sign extend
	component signExtend is
	port (
		i: in   bit_vector(31 downto 0); --input
		o: out  bit_vector(63 downto 0)  --output
	);
	end  component signExtend;
	
	signal regRead2in : bit_vector (4 downto 0);
	signal muxPcOut,muxPcIn, shiftLeft2in, shiftLeft2out, aluMainOut, aluIn2, readData1, readData2, writeData : bit_vector (63 downto 0);
	signal quatro : bit_vector (63 downto 0) := (2 => '1', others => '0');
	signal PC : bit_vector (63 downto 0) := (others => '0');
	signal Ov, Co,z, zb,ovb, cob : bit; -- sinais que completam a ula
	
begin

	RegPC : reg port map(
		clock,
		reset, 
		'1',
		muxPcOut,-- pcOut
		PC
	);

	Regs: regfile port map(
		clock,
		reset,
		regWrite,
		imOut(9 downto 5),
		regRead2in,
		imOut(4 downto 0),
		writeData,
		readData1,
		readData2
	);
	
	aluMain: alu port map(
		readData1,
		aluIn2,
		aluMainOut,
		aluCtrl,
		z, --OUT para a unidade de controle
		Ov,
		Co
	);
	aluPC: alu port map(
		PC,
		muxPcIn,
		muxPcOut,
		"0010",
		zb,
		ovb,
		cob
	);
	
	signEx : signExtend port map(
		imOut, --instrução de memória
		shiftLeft2in
	);
	
	--MUX's
	--pcSomado ou pcSaltado                                  --    
	muxPcIn   <= quatro when pcsrc = '0' else shiftLeft2out;
	--q
	writeData <= aluMainOut when memToReg = '0' else -- entrada de dados para o registrador a ser escrito no banco de registradores
				 dmOut      when memToReg = '1';
	--reg2
	regRead2in <= imOut(20 downto 16) when reg2loc = '0' else
				  imOut (4 downto 0) when reg2loc = '1'; --o segundo registrador do banco ou é tipo r ou tipo d
	--b			  
	aluIn2 <= readData2 when aluSrc = '0' else -- entrada 2 da ula é o segundo registrador para o tipo r
			  shiftLeft2in when aluSrc = '1'; -- ou o signExtend para tipo d	
	
	--extShift2      extendido		  
	shiftLeft2out <= shiftLeft2in(61 downto 0) & "00"; -- pega a saida do signExtend e multiplica por 2


    --OUTPUTS	
	Zero <= z;
	dmAddr <= aluMainOut; --OUT saida da alu principal é o endereço da memória de dados para stur e ldur
	dmIn  <= readData2;  -- OUT 
	imAddr <= PC;         --OUT pc para o proximo ciclo
	opcode <= imOut(31 downto 21); --OUT opcode da instrução vai pra unidade de controle
end architecture;

----------------------------------
--polileg
----------------------------------
library IEEE;
use IEEE.numeric_bit.all;
use ieee.math_real.ceil;
use ieee.math_real.log2;
entity polilegsc is
    port (
        clock, reset : in bit ;
		--data memory
        dmem_addr : out bit_vector (63 downto 0);
        dmem_dati : out bit_vector (63 downto 0);
        dmem_dato : in  bit_vector (63 downto 0);
        dmem_we: out bit;
        --instructrion memory
        imem_addr : out bit_vector (63 downto 0);
        imem_data : in  bit_vector (31 downto 0)
    );
end entity ;
architecture polileg of polilegsc is
component datapath is
    port(
		--Commom
        clock:     in bit;
        reset:     in bit;
		--From Control Unit
        reg2loc:   in bit;
        pcsrc:     in bit;
        memToReg:  in bit;
        aluCtrl:   in bit_vector(3 downto 0);
        aluSrc:    in bit;
        regWrite:  in bit;
		--To controUnit
        opcode:    out bit_vector(10 downto 0);--
        zero:      out bit;--
		--IM interface 
        imAddr:    out bit_vector(63 downto 0);--
        imOut:     in  bit_vector(31 downto 0);
		--DM interface
        dmAddr:    out bit_vector(63 downto 0);--
        dmIn:      out bit_vector(63 downto 0);--
        dmOut:     in  bit_vector(63 downto 0)
    );
end  component datapath;

component alucontrol is
	port (
		aluop : in bit_vector (1 downto 0);
		opcode: in bit_vector (10 downto 0);
		aluCtrl: out bit_vector (3 downto 0)
	);
end component;

component controlunit is
	port (
		--to datapath
		reg2loc      : out bit;
		uncondBranch : out bit;
		branch       : out bit;
		memRead      : out bit;
		memToReg     : out bit;
		aluOp        : out bit_vector(1 downto 0);
		memWrite     : out bit;
		aluSrc       : out bit;
		regWrite     : out bit;
		--from datapath
		opcode       : in bit_vector(10 downto 0)
	);
end component;
	
	signal reg2loc, pcsrc, aluSrc, zero, uncondBranch, branch, memRead, memToReg, memWrite, regWrite: bit;
	signal aluOp : bit_vector (1 downto 0);
	signal aluCtrl : bit_vector (3 downto 0);
	signal opcode : bit_vector (10 downto 0);
	signal imAddr, dmAddr, dmIn, dmOut : bit_vector (63 downto 0);
	signal imOut : bit_vector (31 downto 0);
begin
	DATAP : datapath port map(
		clock, 
		reset,
		reg2loc, 
		pcsrc, 
		memToReg,
		aluCtrl,
		aluSrc,
		regWrite,
		opcode,
		zero,
		imAddr,
		imOut,
		dmAddr,
		dmIn,
		dmOut
	);
	
	ALUCONTROLE: alucontrol port map(
		aluOp,
		opcode,
		aluCtrl
	);
	
	CONTROLE: controlunit port map(
		reg2loc,
		uncondBranch,
		branch,
		memRead,
		memToReg,
		aluOp,
		memWrite,
		aluSrc,
		regWrite,
		opcode
	);
	
	pcsrc <= (uncondBranch or ( branch and zero));
	
	imem_addr <= imAddr;
	imOut <= imem_data;
	
	dmem_we <= memWrite;
	dmem_addr <= dmAddr;
	dmem_dati <= dmIn;
	dmOut <= dmem_dato;
end architecture;