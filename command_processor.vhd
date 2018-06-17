LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.std_logic_unsigned.ALL;
USE ieee.std_logic_misc.ALL;
USE ieee.numeric_std.ALL; 

ENTITY command_processor IS
	PORT
	(
		---------------------------------------
		-- clock and reset
		nreset:		IN STD_LOGIC;
		clk:			IN STD_LOGIC;
		
		status:		OUT STD_LOGIC_VECTOR(1 downto 0) := (OTHERS => '0');
		error:		OUT STD_LOGIC := '0';
		
		---------------------------------------
		-- SPI interface
		spi_tx:		OUT STD_LOGIC_VECTOR(7 downto 0);
		spi_tx_valid:	OUT STD_LOGIC := '0';
		spi_tx_ready:	IN STD_LOGIC;
		
		spi_rx:		IN STD_LOGIC_VECTOR(7 downto 0);
		spi_rx_valid:	IN STD_LOGIC;
		
		---------------------------------------
		-- SRAM write interface
		sram_wr_data:	OUT STD_LOGIC_VECTOR(7 downto 0);
		sram_wr_addr:	OUT STD_LOGIC_VECTOR(17 downto 0);
		sram_wr_req:	OUT STD_LOGIC;
		sram_wr_full:	IN STD_LOGIC;
		
		---------------------------------------
		-- output registers
		out_addr:		OUT STD_LOGIC_VECTOR(17 downto 0);
		out_bytes:		OUT STD_LOGIC_VECTOR(15 downto 0);
		
		-- latch the addr/byte into one of the 16 output units
		out_latch:		OUT STD_LOGIC_VECTOR(15 downto 0)
	);
END command_processor;


ARCHITECTURE SYN OF command_processor IS

---------------------------------------
-- FSM state
TYPE state_t IS (
	IDLE, 
	
	WAIT_OUTPUT_RDY, WAIT_READ_BYTE, CLEAR_SPI,
	
	PARSE_CMD, 
	
	OUTPUT_STATUS_HIGH, OUTPUT_STATUS_LOW,
	
	WRSRAM_ADDRHIGH, WRSRAM_ADDRMID, WRSRAM_ADDRLOW, WRSRAM_DATA, 
	WRSRAM_DATA_WAIT,
	
	REG_CHANNEL, REG_ADDRHIGH, REG_ADDRMID, REG_ADDRLOW,
	REG_BYTESHIGH, REG_BYTESLOW, REG_WRITE
);

-- state to use for the next invocation
SIGNAL state:				state_t;

---------------------------------------
-- state to set when output becomes ready
SIGNAL output_rdy_state:	state_t;
-- timeout counter for output ready
SIGNAL output_rdy_timeout:	STD_LOGIC_VECTOR(8 downto 0) := (OTHERS => '0');

-- state to set when a byte has been read
SIGNAL byte_rx_state:		state_t;
-- timeout counter for byte rx state
SIGNAL byte_rx_timeout:		STD_LOGIC_VECTOR(8 downto 0) := (OTHERS => '0');

---------------------------------------
-- command parsing
SIGNAL command:				STD_LOGIC_VECTOR(1 downto 0);

---------------------------------------
-- status request handling

-- output status bitfield (shifted out for status request)
SIGNAL output_status:		STD_LOGIC_VECTOR(15 downto 0);

---------------------------------------
-- SRAM write handling

-- address to write at
SIGNAL mem_write_addr:		STD_LOGIC_VECTOR(17 downto 0);
-- data to write to that address
SIGNAL mem_write_data:		STD_LOGIC_VECTOR(7 downto 0);

---------------------------------------
-- output register write handling

-- which channel to write output data to
SIGNAL out_reg_channel:		INTEGER RANGE 0 TO 15;
-- address to read data from
SIGNAL out_reg_addr:			STD_LOGIC_VECTOR(17 downto 0);
-- how many bytes to output
SIGNAL out_reg_length:		STD_LOGIC_VECTOR(15 downto 0);

BEGIN

PROCESS (clk, nreset)
BEGIN
	-- Is reset asserted?
	IF nreset = '0' THEN
		-- if so, reset the state machine
		state <= IDLE;
		
		-- output zero bytes
		spi_tx <= (OTHERS => '0');
		spi_tx_valid <= '0';
		
		-- reset the timeouts
		output_rdy_timeout <= (OTHERS => '0');
		byte_rx_timeout <= (OTHERS => '0');
		
		-- reset next states
		byte_rx_state <= IDLE;
		output_rdy_state <= IDLE;
		
		-- reset the write address
		mem_write_addr <= (OTHERS => '0');
		mem_write_data <= (OTHERS => '0');
		
		-- reset SRAM interface
		sram_wr_req <= '0';
		
		sram_wr_data <= (OTHERS => '0');
		sram_wr_addr <= (OTHERS => '0');
		
		-- reset output register bus
		out_addr <= (OTHERS => '0');
		out_bytes <= (OTHERS => '0');
		out_latch <= (OTHERS => '0');
		
		out_reg_channel <= 0;
		out_reg_addr <= (OTHERS => '0');
		out_reg_length <= (OTHERS => '0');
				
		-- clear status
		status <= (OTHERS => '0');
		
		-- clear error status
		error <= '0';
	ELSIF (clk = '1' and clk'event) THEN
		-- otherwise, perform the state
		CASE state IS
			-- Idle; wait for the command to be received
			WHEN IDLE =>
				-- reset timeouts
				output_rdy_timeout <= (OTHERS => '0');
				byte_rx_timeout <= (OTHERS => '0');
				
				-- clear status
				status(0) <= '0';
				status(1) <= '1';
--				status <= (OTHERS => '0');
			
				-- is the SPI RX DATA VALID signal asserted?
				IF (spi_rx_valid = '1') THEN 
					-- if so, copy the command
					command <= spi_rx(1 downto 0);
					
					-- and advance state machine to parse command
					state <= PARSE_CMD;
				END IF;
				
				
				
			-- Wait for output to become ready
			WHEN WAIT_OUTPUT_RDY =>
				-- de-assert the write signal
				spi_tx_valid <= '0';
			
				-- is the output ready?
				IF (spi_tx_ready = '1') THEN
					-- if so, advance the state
					state <= output_rdy_state;
					output_rdy_timeout <= (OTHERS => '0');
				ELSE
					-- if not, increment the timer
					output_rdy_timeout <= output_rdy_timeout + 1;
					
					-- if the timer is expired, go back to idle
					IF and_reduce(output_rdy_timeout) = '1' THEN
						state <= CLEAR_SPI;
						
						-- assert error signal
						error <= '1';
					END IF;
				END IF;
				
			-- Wait for a byte to be received
			WHEN WAIT_READ_BYTE =>
				-- is the output ready?
				IF (spi_rx_valid = '1') THEN
					-- if so, advance the state
					state <= output_rdy_state;
					byte_rx_timeout <= (OTHERS => '0');
				ELSE
					-- if not, increment the timer
					byte_rx_timeout <= byte_rx_timeout + 1;
					
					-- if the timer is expired, go back to idle
					IF and_reduce(byte_rx_timeout) = '1' THEN
						state <= CLEAR_SPI;
						
						-- assert error signal
						error <= '1';
					END IF;
				END IF;
				
			-- Cleans up the SPI output and returns to the idle state.
			WHEN CLEAR_SPI =>
				-- output zero bytes
				spi_tx <= (OTHERS => '0');
				spi_tx_valid <= '0';
				
				-- de-assert any latches
				sram_wr_req <= '0';
				out_latch <= (OTHERS => '0');
				
				-- go back to the idle state
				state <= IDLE;
			
			
			
			-- Only the low two bits of the command are interpreted to determine
			-- what next state to use: 00 outputs the status registers, 01 starts
			-- an SRAM write, and 10 performs an output register write.
			WHEN PARSE_CMD =>
				-- received a command
				status(1) <= '0';
				
				-- clear error
				error <= '0';
			
				-- check the command
				CASE command IS
					-- status request?
					WHEN "00" =>
--						state <= WAIT_OUTPUT_RDY;
--						output_rdy_state <= OUTPUT_STATUS_HIGH;

						state <= OUTPUT_STATUS_HIGH;
					
					-- SRAM write?
					WHEN "01" =>
						state <= WAIT_READ_BYTE;
						output_rdy_state <= WRSRAM_ADDRHIGH;
					
					-- output register write?
					WHEN "10" =>
						state <= WAIT_READ_BYTE;
						output_rdy_state <= REG_CHANNEL;
					
					-- undefined command
					WHEN OTHERS =>
						-- go back to the idle state
						state <= IDLE;
				END CASE;
				
				
				
			-- Output the high byte of the output status register.
			WHEN OUTPUT_STATUS_HIGH =>
				status(0) <= '1';
				
				-- write the high byte of the output status register
				spi_tx <= output_status(15 downto 8);
--				spi_tx <= "11011110";
				spi_tx_valid <= '1';
				
				-- wait for output to become ready before outputting status
				state <= WAIT_OUTPUT_RDY;
				output_rdy_state <= OUTPUT_STATUS_LOW;
				
			-- output the low byte of the output status register
			WHEN OUTPUT_STATUS_LOW =>
				-- write the high byte of the output status register
				spi_tx <= output_status(7 downto 0);
--				spi_tx <= "10101101";
				spi_tx_valid <= '1';
				
				-- go to the clear state
				state <= WAIT_OUTPUT_RDY;
				output_rdy_state <= CLEAR_SPI;
			
			
			
			-- read the high byte of the SRAM write address
			WHEN WRSRAM_ADDRHIGH =>
				mem_write_addr(17 downto 16) <= spi_rx(1 downto 0);
				
				state <= WAIT_READ_BYTE;
				output_rdy_state <= WRSRAM_ADDRMID;
			-- read the middle byte of the SRAM write address
			WHEN WRSRAM_ADDRMID =>
				mem_write_addr(15 downto 8) <= spi_rx;
				
				state <= WAIT_READ_BYTE;
				output_rdy_state <= WRSRAM_ADDRLOW;
			-- read the low byte of the SRAM write address
			WHEN WRSRAM_ADDRLOW =>
				mem_write_addr(7 downto 0) <= spi_rx;
				
				state <= WAIT_READ_BYTE;
				output_rdy_state <= WRSRAM_DATA;
			
			
			-- read a byte of data from the SPI bus and write it to SRAM
			WHEN WRSRAM_DATA =>
				-- make sure write FIFO full signal is not asserted
				IF sram_wr_full = '0' THEN
					-- place SPI rx data on bus, increment address counter
					sram_wr_data <= spi_rx;
					sram_wr_addr <= mem_write_addr;
					sram_wr_req <= '1';
			
					-- wait for data again
					state <= WRSRAM_DATA_WAIT;
				ELSE
					-- make sure the write request signal is not asserted
					sram_wr_req <= '0';
				
					-- the write FIFO is full... wait
					state <= WRSRAM_DATA;
				END IF;
			-- waits for new data to arrive
			WHEN WRSRAM_DATA_WAIT =>
				-- reset memory write strobes
				sram_wr_req <= '0';
				
				-- increment address
				mem_write_addr <= mem_write_addr + 1;
				
				-- wait for data
				state <= WAIT_READ_BYTE;
				output_rdy_state <= WRSRAM_DATA;
				
			
			
			-- Reads the channel to which this data is written
			WHEN REG_CHANNEL =>
				out_reg_channel <= to_integer(signed(spi_rx(3 downto 0)));
				
				state <= WAIT_READ_BYTE;
				output_rdy_state <= REG_ADDRHIGH;
			
			-- Reads the high byte of the address to output data from
			WHEN REG_ADDRHIGH =>
				out_reg_addr(17 downto 16) <= spi_rx(1 downto 0);
				
				state <= WAIT_READ_BYTE;
				output_rdy_state <= REG_ADDRMID;
			
			-- Reads the middle byte of the address
			WHEN REG_ADDRMID =>
				out_reg_addr(15 downto 8) <= spi_rx;
				
				state <= WAIT_READ_BYTE;
				output_rdy_state <= REG_ADDRLOW;
			
			-- Reads the low byte of the address
			WHEN REG_ADDRLOW =>
				out_reg_addr(7 downto 0) <= spi_rx;
				
				state <= WAIT_READ_BYTE;
				output_rdy_state <= REG_BYTESHIGH;
			
			
			-- Reads the high byte of the length
			WHEN REG_BYTESHIGH =>
				out_reg_length(15 downto 8) <= spi_rx;
				
				state <= WAIT_READ_BYTE;
				output_rdy_state <= REG_BYTESLOW;
			
			-- Reads the low byte of the length
			WHEN REG_BYTESLOW =>
				out_reg_length(15 downto 8) <= spi_rx;
				
				-- write the register next cycle
				state <= REG_WRITE;
			
			WHEN REG_WRITE =>
				-- put the data on the output bus
				out_addr <= out_reg_addr;
				out_bytes <= out_reg_length;
				
				
				out_latch(out_reg_channel) <= '1';
			
				-- clear SPI next cycle
				state <= CLEAR_SPI;
				
			
			-- we should not get here
			-- when others =>
		END CASE;
	END IF;
END PROCESS;


END SYN;