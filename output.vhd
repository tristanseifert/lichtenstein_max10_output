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
		
		clk:			IN STD_LOGIC; -- clock for FIFO filler/reading from SRAM
		pwmclk:		IN STD_LOGIC; -- 10MHz clock for generating output waveform
		
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
-- FSM state for FIFO reader (output generator)
TYPE fifo_reader_state_t IS (	
	WAIT_FIFO_NOT_EMPTY, FIFO_DELAY, REQ_BYTE, READ_BYTE, 
	
	OUTPUT_BYTE,
	
	OUTPUT_HIGH, OUTPUT_LOW,
	OUTPUT_HIGH_LAST, OUTPUT_LOW_LAST
);

-- state to use for the next invocation
SIGNAL fifo_reader_state:		fifo_reader_state_t;

---------------------------------------
-- output counters and data

-- number of bits output per byte
SIGNAL bits_output_counter:	INTEGER RANGE 0 TO 7 := 0;
-- byte currently being output
SIGNAL byte_output:				STD_LOGIC_VECTOR(7 downto 0) := (OTHERS => '0');

-- cycle counter for generating waveform
SIGNAL cycle_counter_high:		INTEGER RANGE 0 TO 15 := 0;
SIGNAL cycle_counter_low:		INTEGER RANGE 0 TO 15 := 0;

-- cycle counter for waiting after the FIFO has data and output is started
SIGNAL cycle_counter_start:	INTEGER RANGE 0 TO 7 := 0;

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
SIGNAL test_bytes_written:		STD_LOGIC_VECTOR(7 downto 0) := (OTHERS => '0');
SIGNAL test_counter:				STD_LOGIC_VECTOR(25 downto 0) := (OTHERS => '0');

SIGNAL test_index:				STD_LOGIC_VECTOR(1 downto 0) := (OTHERS => '0');

---------------------------------------
-- test data to fill the FIFO with
TYPE T_DATA IS ARRAY (0 to 15) OF STD_LOGIC_VECTOR(7 downto 0);
CONSTANT test_data : T_DATA := (
	"10000000", "00000000", "00000000", "00000000",
	"00000000", "10000000", "00000000", "00000000",
	"00000000", "00000000", "10000000", "00000000",
	"00000000", "00000000", "00000000", "10000000"

-- for testing the bit widths and timings
--	"10101010", "00000000", "00000000", "00000000",
--	"00000000", "00000000", "00000000", "00000000",
--	"01010101", "00000000", "00000000", "00000000",
--	"11111111", "00000000", "00000000", "00000000"
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
		CASE fifo_reader_state IS
			-- waits for the FIFO to not be empty
			WHEN WAIT_FIFO_NOT_EMPTY =>
				-- is the FIFO not empty?
				IF fifo_empty = '0' THEN
					-- if so, wait a couple cycles to allow for buffers to activate
					cycle_counter_start <= 7;
					fifo_reader_state <= FIFO_DELAY;
					
					-- assert output activity indicator
					pwmout_act <= '1';
				ELSE
					-- wait some more
					fifo_reader_state <= WAIT_FIFO_NOT_EMPTY;
					
					-- pull signal low to initiate reset if there's no more data
					pwmout <= '0';
					
					-- de-assert activity indicator
					pwmout_act <= '0';
				END IF;
				
			-- waits until the cycle counter expires
			WHEN FIFO_DELAY =>
				-- is the timer expired?
				IF cycle_counter_start = 0 THEN
					-- if so, request a byte
					fifo_reader_state <= REQ_BYTE;
				ELSE
					-- if not, decrement the counter
					cycle_counter_start <= cycle_counter_start - 1;
					
					fifo_reader_state <= FIFO_DELAY;
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
--				byte_output <= "10000001";
				bits_output_counter <= 0;
				
				-- begin to output the byte
				fifo_reader_state <= OUTPUT_BYTE;
			
			-- outputs the byte, bit by bit
			WHEN OUTPUT_BYTE =>				
				-- set the output to be high for the first part of the bit
				pwmout <= '1';
				
				
				-- set timings for the output bits
				IF byte_output(7) = '1' THEN
					-- 1 bits are high for 600ns, low for 400ns
					cycle_counter_high <= 4; -- was 5
					cycle_counter_low  <= 5; -- was 3
				ELSE
					-- 0 bits are high for 300ns, low for 900ns
					cycle_counter_high <= 1;
					cycle_counter_low  <= 8;
				END IF;
				
				
				-- is this the last bit to output?
				IF bits_output_counter = 7 THEN
					-- if so, load another byte after the bit is output
					fifo_reader_state <= OUTPUT_HIGH_LAST;
				ELSE										
					-- output the high part of the waveform
					fifo_reader_state <= OUTPUT_HIGH;
					
					-- increment the counter
					bits_output_counter <= bits_output_counter + 1;
				END IF;
				
				
				
			-- output the high part of the signal
			WHEN OUTPUT_HIGH =>
				-- set output high
				pwmout <= '1';
				
				-- has the counter expired?
				IF cycle_counter_high = 0 THEN
					-- if so, output the low part of the signal
					fifo_reader_state <= OUTPUT_LOW;
				ELSE
					-- if not, decrement it
					cycle_counter_high <= cycle_counter_high - 1;
					
					fifo_reader_state <= OUTPUT_HIGH;
				END IF;
			-- output the low part of the signal
			WHEN OUTPUT_LOW =>
				-- set output low
				pwmout <= '0';
			
				-- has the counter expired?
				IF cycle_counter_low = 0 THEN
					-- if so, shift the output sequence left one bit
					byte_output <= byte_output(6 downto 0) & '0';
				
					-- if so, output the next bit
					fifo_reader_state <= OUTPUT_BYTE;
				ELSE
					-- if not, decrement it
					cycle_counter_low <= cycle_counter_low - 1;
					
					fifo_reader_state <= OUTPUT_LOW;
				END IF;
			
		
	
			-- output the high part of the signal
			WHEN OUTPUT_HIGH_LAST =>
				-- set output high
				pwmout <= '1';
				
				-- has the counter expired?
				IF cycle_counter_high = 0 THEN
					-- if so, output the low part of the signal
					fifo_reader_state <= OUTPUT_LOW_LAST;
				ELSE
					-- if not, decrement it
					cycle_counter_high <= cycle_counter_high - 1;
					
					fifo_reader_state <= OUTPUT_HIGH_LAST;
				END IF;
			-- output the low part of the signal
			WHEN OUTPUT_LOW_LAST =>
				-- set output low
				pwmout <= '0';
			
				-- has the counter expired?
				IF cycle_counter_low = 1 THEN
					-- if so, shift the output sequence left one bit
					byte_output <= byte_output(6 downto 0) & '0';
				
					-- is the FIFO empty?
					IF fifo_empty = '0' THEN
						-- if not, assert FIFO read request and read another byte
						fifo_rdreq <= '1';
					
						fifo_reader_state <= READ_BYTE;
					ELSE
						-- TODO: wait 50uS to ensure reset happens?
						-- otherwise, wait for data to come into the FIFO
						fifo_reader_state <= WAIT_FIFO_NOT_EMPTY;
					END IF;
				ELSE
					-- if not, decrement it
					cycle_counter_low <= cycle_counter_low - 1;
					
					fifo_reader_state <= OUTPUT_LOW_LAST;
				END IF;
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
				fifo_din <= test_data(to_integer(unsigned(test_index(1 downto 0)) & unsigned(test_bytes_written(1 downto 0))));
				
				-- go to the next state
				fifo_filler_state <= WAIT_READ_SLOT;
				
			-- de-assert write and wait to write more
			WHEN WAIT_READ_SLOT =>
				fifo_wrreq <= '0';
				
				-- increment counter of bytes written into the FIFO
				test_bytes_written <= test_bytes_written + 1;
				
				-- did we write 16 bytes?
				IF and_reduce(test_bytes_written(5 downto 0)) = '1' THEN
					-- if so, start the wait timer
					test_counter <= (OTHERS => '0');
					fifo_filler_state <= WAIT_READ_VALID;				
				ELSE
					-- if not, write more data
					fifo_filler_state <= IDLE;
				END IF;
				
			-- wait to let the FIFO drain
			WHEN WAIT_READ_VALID=>			
				-- clear number of bytes written
				test_bytes_written <= (OTHERS => '0');
			
				-- increment counter
				test_counter <= test_counter + 1;
				
				-- did the timer expire?
				IF and_reduce(test_counter) = '1' THEN
					-- increment test pattern index timer
					test_index <= test_index + 1;
				
					-- if so, wait for the FIFO to be empty
					fifo_filler_state <= IDLE;
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