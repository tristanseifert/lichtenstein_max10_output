LIBRARY ieee;
USE ieee.std_logic_1164.ALL;

ENTITY sram_controller IS
	PORT
	(
		-- clock and reset
		nreset:		IN STD_LOGIC;
		clk:			IN STD_LOGIC;
		
		-- SRAM bus
		address:		OUT STD_LOGIC_VECTOR(17 downto 0);
		data:			INOUT STD_LOGIC_VECTOR(7 downto 0);
		
		cs:			OUT STD_LOGIC := '1';
		oe:			OUT STD_LOGIC := '1';
		we:			OUT STD_LOGIC := '1';
		
		-- write interface
		wr_clk:		IN STD_LOGIC;
		wr_req:		IN STD_LOGIC := '1';
		wr_data:		IN STD_LOGIC_VECTOR(7 downto 0);
		wr_addr:		IN STD_LOGIC_VECTOR(17 downto 0);
		wr_full:		OUT STD_LOGIC;
		
		-- read interface
		rd_addr:		IN STD_LOGIC_VECTOR(17 downto 0);
		rd_data:		OUT STD_LOGIC_VECTOR(7 downto 0);
		rd_req:		IN STD_LOGIC := '0';
		rd_valid:	OUT STD_LOGIC := '0'
	);
END sram_controller;


ARCHITECTURE SYN OF sram_controller IS

-- SRAM write fifo component (autogenerated)
component sram_write_fifo
	PORT
	(
		aclr		: IN STD_LOGIC  := '0';
		data		: IN STD_LOGIC_VECTOR (25 DOWNTO 0);
		rdclk		: IN STD_LOGIC ;
		rdreq		: IN STD_LOGIC ;
		wrclk		: IN STD_LOGIC ;
		wrreq		: IN STD_LOGIC ;
		q		: OUT STD_LOGIC_VECTOR (25 DOWNTO 0);
		rdempty		: OUT STD_LOGIC ;
		wrfull		: OUT STD_LOGIC 
	);
end component;

-- access to write FIFO
-- these are synchronized to clk
signal fifo_clear		: STD_LOGIC := '0';
signal fifo_read		: STD_LOGIC_VECTOR(25 downto 0);
signal fifo_empty		: STD_LOGIC;
signal fifo_readrq	: STD_LOGIC := '0';

---------------------------------------
-- FSM state
TYPE state_t IS (
	IDLE,
	
	MEM_WRITE, 
	MEM_READ_ADDR, MEM_READ_DATA
);

-- state to use for the next invocation
SIGNAL state:				state_t;

BEGIN

-- instantiate write FIFO
-- reads are synchronized to the clk input, writes to the wr_clk input
writefifo: sram_write_fifo PORT MAP(aclr => fifo_clear, data => wr_addr & wr_data, 
	rdclk => clk, rdreq => fifo_readrq, wrclk => wr_clk, wrreq => wr_req, 
	q => fifo_read, rdempty => fifo_empty, wrfull => wr_full
);

-- state machine
PROCESS (clk, nreset)
BEGIN
	-- Is reset asserted?
	IF nreset = '0' THEN
			-- if so, assert all resets and clear memories
			fifo_clear <= '1';
			
			-- reset state machine
			state <= IDLE;
			
			-- reset SRAM bus
			cs <= '1';
			oe <= '1';
			we <= '1';
	ELSIF (clk = '1' and clk'event) THEN
		-- otherwise, perform the state
		CASE state IS
			-- Idle state; check for write or read requests
			WHEN IDLE =>
				-- de-assert fifo clear
				fifo_clear <= '0';
			
				-- de-assert the chip
				cs <= '1';
				we <= '1';
				
				-- is there a read request?
				IF rd_req = '1' THEN
					-- if so, go into the read state
					state <= MEM_READ_ADDR;
				-- is there a write request?
				ELSIF fifo_empty = '0' THEN
					-- if so, assert the read request on the FIFO and change state
					fifo_readrq <= '1';
					
					state <= MEM_WRITE;
				END IF;
			
			
			
			-- Write state; puts address and data on output
			WHEN MEM_WRITE =>
				-- data on the read bus is no longer valid
				rd_valid <= '0';
				
				-- select the chip, enable write, disable output
				cs <= '0';
				we <= '0';
				oe <= '1';
				
				-- set data and address
				address <= fifo_read(25 downto 8);
				data <= fifo_read(7 downto 0);
				
				-- de-assert read request on the FIFO
				fifo_readrq <= '0';
				
				-- go back to the idle state
				state <= IDLE;
			
			
			
			-- First read state: puts address on the bus
			WHEN MEM_READ_ADDR =>
				-- select the chip, disable write, enable output
				cs <= '0';
				we <= '1';
				oe <= '0';
				
				-- send address
				address <= rd_addr;
				data <= (OTHERS => 'Z');
				
				-- go to the read state
				state <= MEM_READ_DATA;
			
			-- Second read state: reads data from bus
			WHEN MEM_READ_DATA =>
				-- read data
				rd_data <= data;
				rd_valid <= '1';
				
				-- go to idle state
				state <= IDLE;
		END CASE;
	END IF;
END PROCESS;


END SYN;