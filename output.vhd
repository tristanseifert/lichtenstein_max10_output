LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.std_logic_unsigned.ALL;
USE ieee.numeric_std.ALL;
USE ieee.std_logic_misc.ALL;

ENTITY output IS
	PORT
	(
		-- clocks and reset
		nreset:		IN STD_LOGIC;
		
		clk:			IN STD_LOGIC;
		pwmclk:		IN STD_LOGIC;
		
		-- register write bus (from command interpreter)
		reg_addr:	IN STD_LOGIC_VECTOR(17 downto 0);
		reg_length:	IN STD_LOGIC_VECTOR(15 downto 0);
		reg_latch:	IN STD_LOGIC := '0';
		
		-- memory read bus (to memory controller)
		rd_valid:	IN STD_LOGIC := '0';
		rd_addr:		OUT STD_LOGIC_VECTOR(17 downto 0) := (OTHERS => 'Z');
		rd_data:		IN STD_LOGIC_VECTOR(7 downto 0);
		rd_req:		OUT STD_LOGIC := 'Z';
		
		-- set when we're allowed to drive the read bus
		rd_allowed:	IN STD_LOGIC;
		-- high when we would like to perform a read
		rd_desired:	OUT STD_LOGIC := '0';
		
		-- output
		pwmout:		OUT STD_LOGIC := '0';
		pwmout_en:	IN STD_LOGIC := '0';
		
		-- asserted when the output is active
		pwmout_act:	OUT STD_LOGIC := '0';
		
		-- indicates an error condition
		read_error:	OUT STD_LOGIC := '0';
		out_error:	OUT STD_LOGIC := '0'
	);
END output;



ARCHITECTURE SYN OF output IS

---------------------------------------
-- FSM state for FIFO filler
TYPE fifo_filler_state_t IS (
	IDLE,
	
	WAIT_FIFO_NOT_FULL, ASSERT_RDREQ, WAIT_READ_SLOT, DO_READ, WAIT_READ_VALID, 
	HANDLE_COUNTERS
);

-- state to use for the next invocation
SIGNAL fifo_filler_state:		fifo_filler_state_t;

---------------------------------------
-- address to begin output at
SIGNAL read_addr:					STD_LOGIC_VECTOR(17 downto 0);
-- number of bytes to output
SIGNAL read_bytes:				STD_LOGIC_VECTOR(15 downto 0);

---------------------------------------
-- address and length counters
SIGNAL addr_counter:				STD_LOGIC_VECTOR(17 downto 0);
SIGNAL bytes_counter:			STD_LOGIC_VECTOR(15 downto 0);

---------------------------------------
-- FSM state for FIFO reader
TYPE fifo_reader_state_t IS (	
	WAIT_FIFO_NOT_EMPTY, REQ_BYTE, READ_BYTE, OUTPUT_BYTE,
	
	OUTPUT_1, OUTPUT_1_WAIT, OUTPUT_0, OUTPUT_0_WAIT,
	OUTPUT_1_LAST, OUTPUT_1_LAST_WAIT, OUTPUT_0_LAST
);

-- state to use for the next invocation
SIGNAL fifo_reader_state:		fifo_reader_state_t;

---------------------------------------
-- high when the output is active
SIGNAL output_active:			STD_LOGIC := '0';

-- number of bits output per byte
SIGNAL bits_output_counter:	INTEGER RANGE 0 TO 7 := 0;
-- byte currently being output
SIGNAL byte_output:				STD_LOGIC_VECTOR(7 downto 0) := (OTHERS => '0');

---------------------------------------
-- FIFO interface
SIGNAL fifo_clear:				STD_LOGIC := '0';
SIGNAL fifo_rdreq:				STD_LOGIC := '0';
SIGNAL fifo_wrreq:				STD_LOGIC := '0';
SIGNAL fifo_dout:					STD_LOGIC_VECTOR(7 downto 0);
SIGNAL fifo_empty:				STD_LOGIC;
SIGNAL fifo_full:					STD_LOGIC;

SIGNAL fifo_din:					STD_LOGIC_VECTOR(7 downto 0) := "01010101";

---------------------------------------
-- test interface
SIGNAL test_bytes_written:		STD_LOGIC_VECTOR(3 downto 0) := (OTHERS => '0');
SIGNAL test_counter:				STD_LOGIC_VECTOR(22 downto 0) := (OTHERS => '0');

TYPE T_DATA IS ARRAY (0 to 15) OF STD_LOGIC_VECTOR(7 downto 0);
CONSTANT test_data : T_DATA := (
	"10000000", "00000000", "00000000", "00000000",
	"00000000", "10000000", "00000000", "00000000",
	"00000000", "00000000", "10000000", "00000000",
	"00000000", "00000000", "00000000", "10000000"
);

BEGIN

-- instantiate FIFO
fifo: ENTITY work.output_fifo(SYN) PORT MAP(
	aclr => fifo_clear, 
	
	wrclk => clk, wrreq => fifo_wrreq, wrfull => fifo_full,
	data => fifo_din,
	
	rdclk => pwmclk, rdreq => fifo_rdreq, rdempty => fifo_empty,
	q => fifo_dout
);



-- FIFO reading (PWM output) state machine
PROCESS (pwmclk, nreset)
BEGIN
	-- Is reset asserted?
	IF nreset = '0' THEN
			-- clear registers and counters
			bits_output_counter <= 0;
			byte_output <= (OTHERS => '0');
					
			fifo_reader_state <= WAIT_FIFO_NOT_EMPTY;
					
			-- output is not active
			pwmout_act <= '0';
			
			-- clear output error
			out_error <= '0';
	ELSIF (pwmclk = '1' and pwmclk'event) THEN
		out_error <= fifo_empty;
	
		CASE fifo_reader_state IS
			-- waits for the FIFO to not be empty
			WHEN WAIT_FIFO_NOT_EMPTY =>
				-- is the FIFO not empty?
				IF fifo_empty = '0' THEN
					-- if so, request a byte and output it
					fifo_reader_state <= REQ_BYTE;
					
					-- output is active
					pwmout_act <= '1';
				ELSE
					-- wait some more
					fifo_reader_state <= WAIT_FIFO_NOT_EMPTY;
					
					-- the output is not active
					output_active <= '0';
					pwmout <= '0';
					
					pwmout_act <= '0';
				END IF;
			
			-- asserts the FIFO read request line
			WHEN REQ_BYTE =>
				-- assert FIFO read request
				fifo_rdreq <= '1';
				
				-- wait for data to come on the read port
				fifo_reader_state <= READ_BYTE;
			
			-- reads the byte out of the FIFO
			WHEN READ_BYTE =>
				-- de-assert FIFO read request
				fifo_rdreq <= '0';
				
				-- latch data from FIFO and clear output counter
				byte_output <= fifo_dout;
				bits_output_counter <= 0;
				
				-- begin to output the byte
				fifo_reader_state <= OUTPUT_BYTE;
			
			-- outputs the byte, bit by bit
			WHEN OUTPUT_BYTE =>
				-- the output is indeed active
				output_active <= '1';
				
				-- set the output to be high
				pwmout <= '1';
				
				-- is this the last bit to output?
				IF bits_output_counter = 7 THEN
					-- if so, skip the high/low time tracking
					-- is it a 1 bit?
					IF byte_output(7) = '1' THEN
						-- 1 bits are high for two bit times
						fifo_reader_state <= OUTPUT_1_LAST;
					ELSE
						-- 0 bits are high for one bit time
						fifo_reader_state <= OUTPUT_0_LAST;
					END IF;
				ELSE					
					-- is it a 1 bit?
					IF byte_output(7) = '1' THEN
						-- 1 bits are high for two bit times
						fifo_reader_state <= OUTPUT_1;
					ELSE
						-- 0 bits are high for one bit time
						fifo_reader_state <= OUTPUT_0;
					END IF;
					
					-- increment the counter
					bits_output_counter <= bits_output_counter + 1;
				END IF;
				
				
				
			-- when outputting a 0, the signal is high for one bit time only
			-- this is for the case of it being the last bit shifted out
			WHEN OUTPUT_0_LAST =>
				-- make the output low, then read the next byte from the FIFO
				pwmout <= '0';
				
				-- is the FIFO empty?
				IF fifo_empty = '0' THEN
					-- if not, request another byte
					fifo_reader_state <= REQ_BYTE;
				ELSE
					-- otherwise, wait for data to come into the FIFO
					fifo_reader_state <= WAIT_FIFO_NOT_EMPTY;
				END IF;
				
			-- when outputting a 0, the signal is high for one bit time only
			-- this is the general case of the first seven bits
			WHEN OUTPUT_0 =>
				-- make the output low, then wait one more cycle
				pwmout <= '0';
				
				-- shift the output sequence left one bit
				byte_output <= byte_output(6 downto 0) & '0';				
				
				fifo_reader_state <= OUTPUT_0_WAIT;
			-- go back to outputting, we've waited 3 cycles
			WHEN OUTPUT_0_WAIT =>
				fifo_reader_state <= OUTPUT_BYTE;
				
				
				
			-- when outputting a 1, the signal is high for two bit times
			-- this is for the case of it being the last bit shifted out
			WHEN OUTPUT_1_LAST =>
				fifo_reader_state <= OUTPUT_1_LAST_WAIT;
			WHEN OUTPUT_1_LAST_WAIT =>
				pwmout <= '0';		
				
				-- is the FIFO empty?
				IF fifo_empty = '0' THEN
					-- if not, assert FIFO read request and read another byte
					fifo_rdreq <= '1';
				
					fifo_reader_state <= READ_BYTE;
				ELSE
					-- otherwise, wait for data to come into the FIFO
					fifo_reader_state <= WAIT_FIFO_NOT_EMPTY;
				END IF;
				
			-- when outputting a 1, the signal is high for two bit times
			WHEN OUTPUT_1 =>
				-- shift the output sequence left one bit
				byte_output <= byte_output(6 downto 0) & '0';
				
				fifo_reader_state <= OUTPUT_1_WAIT;
			-- go back to outputting, we've waited 3 cycles
			WHEN OUTPUT_1_WAIT =>
				-- make the output low, then wait one more cycle
				pwmout <= '0';		
				
				fifo_reader_state <= OUTPUT_BYTE;
		END CASE;
	END IF;
END PROCESS;



-- test FIFO filler (writes a 16-byte pattern, then waits)
PROCESS (clk, nreset)
BEGIN
	IF nreset = '0' THEN
		-- reset kerjigger
		fifo_filler_state <= IDLE;
		
		-- reset counters
		test_bytes_written <= (OTHERS => '0');
		test_counter <= (OTHERS => '0');
		
		-- reset error flag
		read_error <= '0';
	ELSIF (clk = '1' and clk'event) THEN
		CASE fifo_filler_state IS
			-- idle: wait for the FIFO to not be full
			WHEN IDLE =>
				-- clear error flag
				read_error <= '0';
			
				-- is the FIFO full?
				IF fifo_full = '0' THEN
					-- if not, write data
					fifo_filler_state <= ASSERT_RDREQ;
				ELSE
					-- wait for FIFO to not be full anymore.
					fifo_filler_state <= IDLE;
				END IF;
				
			-- assert the write request signal and write data
			WHEN ASSERT_RDREQ =>
				-- assert write request
				fifo_wrreq <= '1';
				
				-- write the appropriate data from our constant LUT
				fifo_din <= test_data(to_integer(unsigned(test_bytes_written)));
				
				-- go to the next state
				fifo_filler_state <= WAIT_READ_SLOT;
				
			-- de-assert write and wait to write more
			WHEN WAIT_READ_SLOT =>
				fifo_wrreq <= '0';
				
				-- increment counter
				test_bytes_written <= test_bytes_written + 1;
				
				-- did we write 16 bytes?
				IF and_reduce(test_bytes_written) = '1' THEN
					-- if so, start the wait timer
					test_counter <= (OTHERS => '0');
					fifo_filler_state <= WAIT_READ_VALID;				
				ELSE
					-- if not, write more data
					fifo_filler_state <= IDLE;
				END IF;
				
			-- wait to let the FIFO drain
			WHEN WAIT_READ_VALID=>
				-- set error flag
				read_error <= '1';
			
				-- clear number of bytes written
				test_bytes_written <= (OTHERS => '0');
			
				-- increment counter
				test_counter <= test_counter + 1;
				
				-- did the timer expire?
				IF test_counter(22) = '1' THEN
					-- if so, repeat
					fifo_filler_state <= IDLE;
--					fifo_filler_state <= WAIT_READ_VALID;
				ELSE
					-- wait some more lol
					fifo_filler_state <= WAIT_READ_VALID;
				END IF;
				
			-- catch-all (shouldn't happen)
			WHEN OTHERS =>
				fifo_filler_state <= IDLE;
		END CASE;
	END IF;
END PROCESS;



-- FIFO filling state machine
--PROCESS (clk, nreset)
--BEGIN
--	-- Is reset asserted?
--	IF nreset = '0' THEN
--			-- clear registers and counters
--			read_addr <= (OTHERS => '0');
--			read_bytes <= (OTHERS => '0');
--			
--			addr_counter <= read_addr;
--			bytes_counter <= read_bytes;
--			
--			-- clear FIFO
--			fifo_clear <= '1';
--			
--			-- reset state machine
--			fifo_filler_state <= IDLE;
--	ELSIF (clk = '1' and clk'event) THEN
--		CASE fifo_filler_state IS
--			-- Idle state, wait for the register data to be latched
--			WHEN IDLE =>
--				-- de-assert FIFO clear
--				fifo_clear <= '0';
--			
--				-- is the latch strobe high?
--				IF reg_latch = '1' THEN
--					-- latch the byte length and address
--					read_addr <= reg_addr;
--					bytes_counter <= reg_length;
--					
--					-- reset counters
--					addr_counter <= read_addr;
--					bytes_counter <= read_bytes;
--					
--					-- begin reading
--					fifo_filler_state <= WAIT_FIFO_NOT_FULL;
--				ELSE
--					-- wait some more
--					fifo_filler_state <= IDLE;
--				END IF;
--				
--				-- Wait for the FIFO to not be full
--				WHEN WAIT_FIFO_NOT_FULL =>
--					IF fifo_full = '0' THEN
--						fifo_filler_state <= ASSERT_RDREQ;
--					ELSE
--						-- wait some more
--						fifo_filler_state <= WAIT_FIFO_NOT_FULL;
--					END IF;
--				
--				-- Assert the read request signal
--				WHEN ASSERT_RDREQ =>
--					rd_desired <= '1';
--					
--					fifo_filler_state <= WAIT_READ_SLOT;
--				-- Wait for a read slot
--				WHEN WAIT_READ_SLOT =>
--					-- is the read ack asserted?
--					IF rd_allowed = '1' THEN
--						-- if so, assert the read request and send address
--						rd_addr <= addr_counter;
--						rd_req <= '1';
--						
--						-- wait for data to be valid
--						fifo_filler_state <= WAIT_READ_VALID;
--					ELSE
--						-- if not, wait more
--						fifo_filler_state <= WAIT_READ_SLOT;
--					END IF;
--				-- wait for the data to be valid
--				WHEN WAIT_READ_VALID =>
--					-- is the read data valid?
--					IF rd_valid = '1' THEN
--						-- if so, read the data
--						fifo_filler_state <= DO_READ;
--					ELSE
--						-- if not, wait for it to be valid
--						fifo_filler_state <= WAIT_READ_VALID;
--					END IF;
--					
--				-- push the read data into the FIFO
--				WHEN DO_READ =>
--					-- stop driving address bus
--					rd_addr <= (OTHERS => 'Z');
--					rd_req <= 'Z';
--					
--					-- disable read flag
--					rd_desired <= '0';
--					
--					-- assert the FIFO write request
--					fifo_wrreq <= '1';
--					
--					-- manage the counters
--					fifo_filler_state <= HANDLE_COUNTERS;
--					
--				-- increment address, decrement length
--				WHEN HANDLE_COUNTERS =>
--					-- deassert the FIFO write request
--					fifo_wrreq <= '0';
--					
--					-- increment address, decrement bytes
--					addr_counter <= addr_counter + 1;
--					bytes_counter <= bytes_counter - 1;
--					
--					-- if the bytes counter is zero, stop reading
--					IF bytes_counter = (bytes_counter'range => '0') THEN
--						fifo_filler_state <= IDLE;
--					ELSE
--						-- otherwise, read more data into the FIFO
--						fifo_filler_state <= WAIT_FIFO_NOT_FULL;
--					END IF;					
--		END CASE;
--	END IF;
--END PROCESS;

END SYN;