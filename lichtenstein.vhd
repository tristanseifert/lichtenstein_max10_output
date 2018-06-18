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
			pwm_out_enable:	OUT STD_LOGIC_VECTOR(1 downto 0) := (OTHERS => '0')
		);
END lichtenstein;



ARCHITECTURE SYN OF lichtenstein IS

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

SIGNAL out_read_error				: STD_LOGIC_VECTOR(15 downto 0) := (OTHERS => '0');
SIGNAL out_pwm_error					: STD_LOGIC_VECTOR(15 downto 0) := (OTHERS => '0');

-- output read arbiter
SIGNAL out_arbiter_nreset			: STD_LOGIC := '1';


-- timer for blinking heartbeat LED
SIGNAL heartbeat_timer				: STD_LOGIC_VECTOR(20 downto 0) := (OTHERS => '0');

BEGIN


-- instantiate main PLL
clocks: ENTITY work.mainpll(SYN) PORT MAP(areset => pll_reset, inclk0 => clkin, c0 => clk_24, 
	c1 => clk_48, c2 => pwmclk, locked => pll_locked
);

-- SRAM controller
sram: ENTITY work.sram_controller(SYN) PORT MAP(nreset => sram_nreset, clk => clk_48, 
	
	address => sram_addr, data => sram_data, cs => sram_ce, 
	oe => sram_oe, we => sram_we, 
	
	wr_clk => clk_48, wr_req => sram_wrreq, wr_data => sram_wrdata, 
	wr_addr => sram_wraddr, wr_full => sram_wrfull,
	
	rd_addr => sram_rdaddr, rd_data => sram_rddata, rd_req => sram_rdreq,
	rd_valid => sram_rdvalid,
	
	status => sram_status
);


-- SPI slave (mode 0)
Inst_spi_slave: ENTITY work.spi_slave(rtl)
	GENERIC MAP(N => 8, CPOL => '0', CPHA => '0', PREFETCH => 2)
	PORT MAP(
		clk_i => clk_48,
		spi_ssel_i => spi_cs,
		spi_sck_i => spi_sck,
		spi_mosi_i => spi_mosi,
		spi_miso_o => spi_miso,

		-- request for more data (write new tx data when high)
		di_req_o => spi_tx_ready,
		-- transmit data
		di_i => spi_tx,
		-- write request (assert that tx data is valid)
		wren_i => spi_tx_valid,

		-- rx data valid
		do_valid_o => spi_rx_valid,
		-- rx data
		do_o => spi_rx
);

-- command processor
cmd: ENTITY work.command_processor(SYN) PORT MAP(nreset => cmd_nreset, clk => clk_48,

	status => cmd_status, error => cmd_error,
	
	spi_tx => spi_tx, spi_tx_valid => spi_tx_valid, spi_rx => spi_rx,
	spi_tx_ready => spi_tx_ready,	spi_rx_valid => spi_rx_valid, 
	
	sram_wr_data => sram_wrdata, sram_wr_addr => sram_wraddr, 
	sram_wr_req => sram_wrreq, sram_wr_full => sram_wrfull,
	
	out_addr => out_reg_addr, out_bytes => out_reg_bytes, 
	out_latch => out_reg_latch, out_active => out_active
);

-- read arbiter
arbiter: ENTITY work.read_arbiter(SYN) PORT MAP(
	nreset => out_arbiter_nreset, clk => clk_48, 
	
	rdreq_in => out_rd_desired, rdreq_ack => out_rd_allowed
);


-- instantiate output channels
GEN_OUT:
FOR I IN 1 to 1 GENERATE
	OUTX: ENTITY work.output(SYN) PORT MAP(
		nreset => out_nreset, clk => clk_48, pwmclk => pwmclk,
		
		reg_addr => out_reg_addr, reg_length => out_reg_bytes, 
		reg_latch => out_reg_latch(I),
		
		rd_valid => sram_rdvalid,
		rd_addr => sram_rdaddr, rd_data => sram_rddata, rd_req => sram_rdreq,
		rd_allowed => out_rd_allowed(I), rd_desired => out_rd_desired(I),
		
		pwmout => pwm_out(I), pwmout_en => ledout_en,
		
		pwmout_act => out_active(I),
		
		read_error => out_read_error(I), out_error => out_pwm_error(I)
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
		
--		status(3) <= heartbeat_timer(20);
	END IF;
END PROCESS;

-- status LED 1 is active if ANY channel is outputting data
-- also, synthesize the output drivers' OE signals
PROCESS (pwmclk) IS
BEGIN
	IF rising_edge(pwmclk) THEN
		status(3) <= or_reduce(out_active);
		
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
--		status(1) <= spi_mosi;
--		status(0) <= NOT(spi_cs); -- 1 when chip select is 0
		status(0) <= cmd_error; -- timeout (or other error)
		
--		status(1) <= cmd_status(0); -- processing cmd
--		status(2) <= cmd_status(1); -- idle
	END IF;
END PROCESS;

END SYN;