LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.std_logic_unsigned.ALL;
USE ieee.numeric_std.ALL; 

ENTITY read_arbiter IS
	PORT
	(
		-- clocks and reset
		nreset:		IN STD_LOGIC;
		clk:			IN STD_LOGIC;
		
		-- read request signals from output nodes
		rdreq_in:	IN STD_LOGIC_VECTOR(15 downto 0);
		-- read request ack signals to output nodes
		rdreq_ack:	OUT STD_LOGIC_VECTOR(15 downto 0) := (OTHERS => '0')
	);
END read_arbiter;


ARCHITECTURE SYN OF read_arbiter IS

---------------------------------------
-- FSM state for FIFO filler
TYPE state_t IS (
	CHECK_CHANNEL, WAIT_CHANNEL, NEXT_CHANNEL
);

-- state to use for the next invocation
SIGNAL state:						state_t;

---------------------------------------
-- channel to check
SIGNAL channel:					INTEGER RANGE 0 TO 15 := 0;

BEGIN

-- FIFO reading state machine
PROCESS (clk, nreset)
BEGIN
	-- Is reset asserted?
	IF nreset = '0' THEN
			-- reset acknowledgement signals
			rdreq_ack <= (OTHERS => '0');
	
			-- clear registers and counters
			channel <= 0;
			
			-- reset state machine
			state <= CHECK_CHANNEL;
	ELSIF (clk = '1' and clk'event) THEN
		CASE state IS
			-- check whether the specified channel requests a read
			WHEN CHECK_CHANNEL =>
				IF (rdreq_in(channel) = '1') THEN
					state <= WAIT_CHANNEL;
				ELSE
					state <= NEXT_CHANNEL;
				END IF;
				
			-- waits for the channel to be done with the read
			WHEN WAIT_CHANNEL =>				
				-- is read request still asserted?
				IF (rdreq_in(channel) = '1') THEN
					-- if so, hold the ack and wait
					rdreq_ack(channel) <= '1';
					state <= WAIT_CHANNEL;
				ELSE
					-- otherwise, clear it and check the next channel
					rdreq_ack(channel) <= '0';
					state <= NEXT_CHANNEL;
				END IF;
			
			-- increments the channel counter
			WHEN NEXT_CHANNEL =>
				channel <= channel + 1;
				
				state <= CHECK_CHANNEL;
		END CASE;
	END IF;
END PROCESS;

END SYN;
