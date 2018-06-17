LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.std_logic_unsigned.ALL;
USE ieee.numeric_std.ALL; 
USE ieee.std_logic_misc.ALL;

ENTITY lichtenstein IS
      PORT(
			-- 24MHz main clock
			clkin:		  IN STD_LOGIC;
		
			-- SRAM bus
			sram_data: INOUT STD_LOGIC_VECTOR(7 downto 0);
			sram_addr:   OUT STD_LOGIC_VECTOR(17 downto 0);
			sram_oe:		 OUT STD_LOGIC;
			sram_ce:		 OUT STD_LOGIC;
			sram_we:		 OUT STD_LOGIC;
			
			-- SPI bus
			spi_cs:		  IN STD_LOGIC;
			spi_mosi:	  IN STD_LOGIC;
			spi_miso:	 OUT STD_LOGIC := 'Z';
			spi_sck:		  IN STD_LOGIC;
			
			-- strobes from host
			ledout_en:	  IN STD_LOGIC;
			ledout_rst:	  IN STD_LOGIC;
			
			-- status outputs
			status:		 OUT STD_LOGIC_VECTOR(3 downto 0) := (OTHERS => '0');
			
			-- PWM output
			pwm_out:		 OUT STD_LOGIC_VECTOR(15 downto 0) := (OTHERS => '0');
			-- output enables for each bank of 8
			pwm_out_enable:	OUT STD_LOGIC_VECTOR(1 downto 0) := (OTHERS => '1')
		);
END lichtenstein;

ARCHITECTURE SYN OF lichtenstein IS

-- sram controller
COMPONENT sram_controller
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
		rd_valid:	OUT STD_LOGIC := '0';
		
		-- controller status: (00 idle, 01 write, 10 read)
		status:		OUT STD_LOGIC_VECTOR(1 downto 0)
	);
END COMPONENT;


-- SPI slave controller
component SPI_SLAVE is
    PORT (
        CLK      : in  std_logic; -- system clock
        RST      : in  std_logic; -- high active synchronous reset
        -- SPI SLAVE INTERFACE
        SCLK     : in  std_logic; -- SPI clock
        CS_N     : in  std_logic; -- SPI chip select, active in low
        MOSI     : in  std_logic; -- SPI serial data from master to slave
        MISO     : out std_logic; -- SPI serial data from slave to master
        -- USER INTERFACE
        DIN      : in  std_logic_vector(7 downto 0); -- input data for SPI master
        DIN_VLD  : in  std_logic; -- when DIN_VLD = 1, input data are valid
        READY    : out std_logic; -- when READY = 1, valid input data are accept
        DOUT     : out std_logic_vector(7 downto 0); -- output data from SPI master
        DOUT_VLD : out std_logic  -- when DOUT_VLD = 1, output data are valid
    );
END COMPONENT;

-- command processor
COMPONENT command_processor
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
END COMPONENT;

-- output block
COMPONENT output IS
	PORT
	(
		-- clocks and reset
		nreset:		IN STD_LOGIC;
		clk:			IN STD_LOGIC;
		pwmclk:		IN STD_LOGIC;
		
		-- register bus
		reg_addr:	IN STD_LOGIC_VECTOR(17 downto 0);
		reg_length:	IN STD_LOGIC_VECTOR(15 downto 0);
		reg_latch:	IN STD_LOGIC := '0';
		
		-- memory bus
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
		pwmout_act:	OUT STD_LOGIC := '0'
	);
END COMPONENT;

-- output read arbiter
COMPONENT read_arbiter IS
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
END COMPONENT;

-- autogenerated component (mainpll.cmp)
COMPONENT mainpll
	PORT
	(
		areset		: IN STD_LOGIC  := '0';
		inclk0		: IN STD_LOGIC  := '0';
		c0		: OUT STD_LOGIC ;
		c1		: OUT STD_LOGIC ;
		c2		: OUT STD_LOGIC ;
		locked		: OUT STD_LOGIC 
	);
END COMPONENT;

-- output clocks from PLL
SIGNAL clk_24, clk_48, pwmclk	: std_logic;
SIGNAL pll_reset						: std_logic := '0';
SIGNAL pll_locked						: std_logic;

-- global reset, active 0
SIGNAL gReset							: std_logic := '1';

-- SRAM controller internal
signal sram_nreset					: std_logic;

-- SRAM write bus
signal sram_wrdata					: STD_LOGIC_VECTOR(7 downto 0);
signal sram_wraddr					: STD_LOGIC_VECTOR(17 downto 0);
signal sram_wrreq, sram_wrfull	: STD_LOGIC;

-- SRAM read bus
SIGNAL sram_rdaddr					: STD_LOGIC_VECTOR(17 downto 0);
SIGNAL sram_rddata					: STD_LOGIC_VECTOR(7 downto 0);
SIGNAL sram_rdreq						: STD_LOGIC := '0';
SIGNAL sram_rdvalid					: STD_LOGIC;

-- SRAM status
signal sram_status					: STD_LOGIC_VECTOR(1 downto 0);

-- SPI interface
signal spi_miso_tmp					: STD_LOGIC;

signal spi_reset						: STD_LOGIC := '0';

signal spi_tx							: STD_LOGIC_VECTOR(7 downto 0);
signal spi_tx_valid					: STD_LOGIC := '0';
signal spi_tx_ready					: STD_LOGIC;

signal spi_rx							: STD_LOGIC_VECTOR(7 downto 0);
signal spi_rx_valid					: STD_LOGIC;

-- command processor
signal cmd_nreset						: STD_LOGIC := '1';

signal cmd_status						: STD_LOGIC_VECTOR(1 downto 0);
signal cmd_error						: STD_LOGIC;


-- output register bus
SIGNAL out_reg_addr					: STD_LOGIC_VECTOR(17 downto 0);
SIGNAL out_reg_bytes					: STD_LOGIC_VECTOR(15 downto 0);
SIGNAL out_reg_latch					: STD_LOGIC_VECTOR(15 downto 0);

-- output blocks
SIGNAL out_nreset						: STD_LOGIC := '1';

SIGNAL out_rd_allowed				: STD_LOGIC_VECTOR(15 downto 0) := (OTHERS => '0');
SIGNAL out_rd_desired				: STD_LOGIC_VECTOR(15 downto 0);

SIGNAL out_active						: STD_LOGIC_VECTOR(15 downto 0) := (OTHERS => '0');

-- output read arbiter
SIGNAL out_arbiter_nreset			: STD_LOGIC := '1';


-- timer for blinking heartbeat LED
SIGNAL heartbeat_timer				: STD_LOGIC_VECTOR(20 downto 0) := (OTHERS => '0');

BEGIN

-- instantiate main PLL
clocks: mainpll PORT MAP(areset => pll_reset, inclk0 => clkin, c0 => clk_24, 
	c1 => clk_48, c2 => pwmclk, locked => pll_locked
);

-- SRAM controller
sram: sram_controller PORT MAP(nreset => sram_nreset, clk => clk_48, 
	
	address => sram_addr, data => sram_data, cs => sram_ce, 
	oe => sram_oe, we => sram_we, wr_clk => clk_24, 
	
	wr_req => sram_wrreq, wr_data => sram_wrdata, 
	wr_addr => sram_wraddr, wr_full => sram_wrfull,
	
	rd_addr => sram_rdaddr, rd_data => sram_rddata, rd_req => sram_rdreq,
	rd_valid => sram_rdvalid,
	
	status => sram_status
);

-- SPI slave
--spi_miso <= 'Z';

spi: SPI_SLAVE PORT MAP(CLK => clk_48, RST => spi_reset, SCLK => spi_sck,
	CS_N => spi_cs, MOSI => spi_mosi, MISO => spi_miso, DIN => spi_tx,
	DIN_VLD => spi_tx_valid, READY => spi_tx_ready, DOUT => spi_rx,
	DOUT_VLD => spi_rx_valid
);

-- command processor
cmd: command_processor PORT MAP(nreset => cmd_nreset, clk => clk_48,

	status => cmd_status, error => cmd_error,
	
	spi_tx => spi_tx, spi_tx_valid => spi_tx_valid, spi_rx => spi_rx,
	spi_tx_ready => spi_tx_ready,
	spi_rx_valid => spi_rx_valid, sram_wr_data => sram_wrdata, 
	
	sram_wr_addr => sram_wraddr, sram_wr_req => sram_wrreq,
	sram_wr_full => sram_wrfull,
	
	out_addr => out_reg_addr, out_bytes => out_reg_bytes, 
	out_latch => out_reg_latch
);

-- read arbiter
arbiter: read_arbiter PORT MAP(
	nreset => out_arbiter_nreset, clk => clk_48, 
	
	rdreq_in => out_rd_desired, rdreq_ack => out_rd_allowed
);


-- instantiate output channels
GEN_OUT:
FOR I IN 0 to 1 GENERATE
	OUTX: output PORT MAP(
		nreset => out_nreset, clk => clk_48, pwmclk => pwmclk,
		
		reg_addr => out_reg_addr, reg_length => out_reg_bytes, 
		reg_latch => out_reg_latch(I),
		
		rd_valid => sram_rdvalid,
		rd_addr => sram_rdaddr, rd_data => sram_rddata, rd_req => sram_rdreq,
		rd_allowed => out_rd_allowed(I), rd_desired => out_rd_desired(I),
		
		pwmout => pwm_out(I), pwmout_en => ledout_en,
		
		pwmout_act => out_active(I)
	);
END GENERATE GEN_OUT;



-- assert internal reset if external reset signal is asserted
PROCESS (clk_24) IS
BEGIN

	IF rising_edge(clk_24) THEN
		-- is the reset strobe asserted?
		IF ledout_rst = '0' THEN
			-- if so, assert reset
			gReset <= '0';
		ELSE
			-- de-assert reset
			gReset <= '1';
		END IF;
	END IF;
END PROCESS;

-- assert resets of all components if internal reset is asserted
PROCESS (clk_24) IS
BEGIN
	IF rising_edge(clk_24) THEN
		-- is reset low?
		IF gReset = '0' THEN
			-- if so, assert resets
			sram_nreset <= '0';
			cmd_nreset <= '0';
			out_nreset <= '0';
			out_arbiter_nreset <= '0';

			spi_reset <= '1';
		ELSE
			-- de-assert resets
			sram_nreset <= '1';
			cmd_nreset <= '1';
			out_nreset <= '1';
			out_arbiter_nreset <= '1';

			spi_reset <= '0';
		END IF;
	END IF;
END PROCESS;

-- status LED 0 is a heartbeat
PROCESS (pwmclk) IS
BEGIN
	IF rising_edge(pwmclk) THEN
		heartbeat_timer <= heartbeat_timer + 1;
		
		status(0) <= heartbeat_timer(20);
	END IF;
END PROCESS;

-- status LED 1 is active if ANY channel is outputting data
-- also, synthesize the output drivers' OE signals
PROCESS (pwmclk) IS
BEGIN
	IF rising_edge(pwmclk) THEN
--		status(1) <= or_reduce(out_active);
		
		pwm_out_enable(0) <= or_reduce(out_active(7 downto 0));
		pwm_out_enable(1) <= or_reduce(out_active(15 downto 8));
	END IF;
END PROCESS;

-- status LED 2 is indicating the status of SRAM controller (1 = write)
--PROCESS (clk_48) IS
--BEGIN
--	IF rising_edge (pwmclk) THEN
--		status(2) <= sram_status(0);
--	END IF;
--END PROCESS;

-- status LED 3 indicates when we received a command
PROCESS (clk_48) IS
BEGIN
	IF rising_edge (pwmclk) THEN
		status(2) <= cmd_status(0); 
		
		status(3) <= cmd_status(1); -- idle
		
		status(1) <= cmd_error;
	END IF;
END PROCESS;

END SYN;